/// Given a JSON schema generate a Pulumi schema, and a way to read/write from that pulumi type to the expected json.
module JsonSchema.Converter

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Text.Json
open System.Text.Json.Nodes


type KeywordCollection = IReadOnlyCollection<Json.Schema.IJsonSchemaKeyword>    

let pickKeyword<'T when 'T :> Json.Schema.IJsonSchemaKeyword> (keywords : KeywordCollection) : 'T option =
    let picked = 
        keywords
        |> Seq.choose (function | :? 'T as t -> Some t | _ -> None)
        |> Seq.toArray

    // Error if we see more than 1 of the same type of keyword, mostly for sanity
    if picked.Length > 1 then
        failwithf "Found more than one keyword of type %s" (typeof<'T>.FullName)

    if picked.Length = 1 then
        Some picked[0]
    else 
        None

let allPrefixes (list : 'T list) : 'T list seq =
    Seq.init list.Length (fun i ->
        List.take (i+1) list
    )

let isValidationKeyword (keyword : Json.Schema.IJsonSchemaKeyword) : bool =
    match keyword with
    | :? Json.Schema.TitleKeyword
    | :? Json.Schema.CommentKeyword
    | :? Json.Schema.DescriptionKeyword
    | :? Json.Schema.DefsKeyword -> false
    | _ -> true

let optionOfTry<'T> (tryResult : bool * 'T) : 'T option =
    match tryResult with 
    | false, _ -> None
    | true, value -> Some value

let split (l : list<'T>) (splitter : 'T -> Choice<'U, 'V>) : 'U list * 'V list = 
    List.foldBack (fun item (us, vs) -> 
        match splitter item with
        | Choice1Of2 u -> (u :: us, vs)
        | Choice2Of2 v -> (us, v :: vs)
    ) l ([], [])

// Given a list of results, either return an OK of the seq or an Error
let okList (l : list<Result<'T,'TError>>) : Result<list<'T>, list<'TError>> =
    let oks, errors = 
        split l (fun r -> 
            match r with 
            | Result.Ok ok -> Choice1Of2 ok
            | Result.Error err -> Choice2Of2 err
        )
    match errors with 
    | [] -> Result.Ok oks
    | errors -> Result.Error errors

let errorf format =
    Printf.kprintf Result<'t, string>.Error format

let cleanTextForName (text : string) : string =
    // replace all "bad chars" with _, then do a snake case to camel case, then normalize
    let text = 
        text
            .Replace(":", "_")
            .Replace("/", "_")
            .Replace(" ", "_")
            .Replace("-", "_")
            .Trim('_')
    let result = System.Text.StringBuilder()
    let mutable case = false
    let mutable first = true
    for ch in text do 
        if first then 
            result.Append (string (Char.ToLower ch)) |> ignore
            first <- false
        else 
            if ch = '_' then
                case <- true
            elif Char.IsDigit ch then
                result.Append (string ch) |> ignore
                case <- true
            elif case then
                result.Append (string (Char.ToUpper ch)) |> ignore
                case <- false
            else 
                result.Append (string ch) |> ignore

    result.ToString()

let tryGetNumeric (value : JsonElement) = 
    if value.ValueKind = JsonValueKind.Number then 
        match value.TryGetInt64() with
        | true, i -> Some (Choice1Of2 i)
        | false, _ ->
            match value.TryGetDouble() with
            | true, n -> Some (Choice2Of2 n)
            | false, _ -> None
    else None

[<RequireQualifiedAccess>]
type PrimitiveType = Boolean | Integer | Number | String
with 
    member this.JsonValue = 
        match this with 
        | Boolean -> JsonValue.Create("boolean")
        | Integer -> JsonValue.Create("integer")
        | Number -> JsonValue.Create("number")
        | String -> JsonValue.Create("string")

// The schema keywords that can apply to string validation
type StringValidation = {
    MinLength : uint option
    MaxLength : uint option
    Pattern : string option
} with 
    static member None = 
        {
            MinLength = None
            MaxLength = None
            Pattern = None
        }

    static member FromKeywords (keywords : KeywordCollection) =
        {
            MinLength = pickKeyword<Json.Schema.MinLengthKeyword> keywords |> Option.map (fun kw -> kw.Value)
            MaxLength = pickKeyword<Json.Schema.MaxLengthKeyword> keywords |> Option.map (fun kw -> kw.Value)
            Pattern = pickKeyword<Json.Schema.PatternKeyword> keywords |> Option.map (fun kw -> kw.Value.ToString())
        }

    member this.Validate (value : string) =
        let minCheck =
            match this.MinLength with 
            | Some l when uint value.Length < l -> 
                sprintf "Value is not longer than or equal to %d characters" l
                |> Some
            | _ -> None
        let maxCheck =
            match this.MaxLength with 
            | Some l when uint value.Length > l -> 
                sprintf "Value is not shorter than or equal to %d characters" l
                |> Some
            | _ -> None
        let patternCheck =
            match this.Pattern with 
            | Some pattern ->
                let re = System.Text.RegularExpressions.Regex(pattern, System.Text.RegularExpressions.RegexOptions.ECMAScript)
                if re.IsMatch value |> not then 
                    Some "The string value was not a match for the indicated regular expression"
                else 
                    None
            | _ -> None

        Option.orElse patternCheck (Option.orElse maxCheck minCheck)
        
// The schema keywords that can apply to numeric validation
type NumericValidation = {
    MultipleOf : decimal option
    Maximum : decimal option
    ExclusiveMaximum : decimal option
    Minimum : decimal option
    ExclusiveMinimum : decimal option
} with 
    static member None = 
        {
            MultipleOf = None
            Maximum = None
            ExclusiveMaximum = None
            Minimum = None
            ExclusiveMinimum = None
        }

    static member FromKeywords (keywords : KeywordCollection) =
        {
            MultipleOf = pickKeyword<Json.Schema.MultipleOfKeyword> keywords |> Option.map (fun kw -> kw.Value)
            Maximum = pickKeyword<Json.Schema.MaximumKeyword> keywords |> Option.map (fun kw -> kw.Value)
            ExclusiveMaximum = pickKeyword<Json.Schema.ExclusiveMaximumKeyword> keywords |> Option.map (fun kw -> kw.Value)
            Minimum = pickKeyword<Json.Schema.MinimumKeyword> keywords |> Option.map (fun kw -> kw.Value)
            ExclusiveMinimum = pickKeyword<Json.Schema.ExclusiveMinimumKeyword> keywords |> Option.map (fun kw -> kw.Value)
        }

    member this.Validate (value : decimal) =
        let mulCheck =
            match this.MultipleOf with 
            | Some m when Decimal.Remainder(value, m) <> 0M -> 
                sprintf "%M is not a multiple of %M" value m
                |> Some
            | _ -> None

        let maxCheck =
            match this.Maximum with 
            | Some m when value > m ->
                sprintf "%M is not less than or equal to %M" value m
                |> Some
            | _ -> None

        let emaxCheck =
            match this.ExclusiveMaximum with 
            | Some m when value >= m ->
                sprintf "%M is not less than %M" value m
                |> Some
            | _ -> None

        let minCheck =
            match this.Minimum with 
            | Some m when value < m ->
                sprintf "%M is not greater than or equal to  %M" value m
                |> Some
            | _ -> None
            
        let eminCheck =
            match this.ExclusiveMinimum with 
            | Some m when value <= m ->
                sprintf "%M is not greater than %M" value m
                |> Some
            | _ -> None

        None 
        |> Option.orElse mulCheck 
        |> Option.orElse maxCheck 
        |> Option.orElse emaxCheck 
        |> Option.orElse minCheck 
        |> Option.orElse eminCheck 

type PrimitiveValidation = {
    String : StringValidation
    Numeric : NumericValidation
    Const : JsonNode option
} with 
    static member None = { 
        String = StringValidation.None
        Numeric = NumericValidation.None
        Const = None
    }

    static member FromKeywords (keywords : KeywordCollection) =
        {
            String = StringValidation.FromKeywords keywords
            Numeric = NumericValidation.FromKeywords keywords
            Const = keywords |> pickKeyword<Json.Schema.ConstKeyword> |> Option.map (fun kw -> kw.Value)
        }

    member this.Validate (node : JsonNode option) =
        match this.Const with 
        | Some c -> 
            if Json.More.JsonNodeEqualityComparer.Instance.Equals(c, Option.toObj node) then None
            else Some (sprintf "Expected %s" (c.ToJsonString()))
        | None -> None
        
type ArrayValidation = {
    MinItems : uint option
    MaxItems : uint option
} with 
    static member None = 
        {
            MinItems = None
            MaxItems = None
        }

    static member FromKeywords (keywords : KeywordCollection) =
        {
            MinItems = pickKeyword<Json.Schema.MinItemsKeyword> keywords |> Option.map (fun kw -> kw.Value)
            MaxItems = pickKeyword<Json.Schema.MaxItemsKeyword> keywords |> Option.map (fun kw -> kw.Value)
        }

    member this.Validate (value : JsonElement) =
        if value.ValueKind <> System.Text.Json.JsonValueKind.Array then None 
        else
            let count = value.GetArrayLength()

            let minCheck =
                match this.MinItems with 
                | Some l when uint count < l -> 
                    sprintf "Value has fewer than %d items" l
                    |> Some
                | _ -> None
            let maxCheck =
                match this.MaxItems with 
                | Some l when uint count > l -> 
                    sprintf "Value has more than %d items" l
                    |> Some
                | _ -> None
            
            Option.orElse maxCheck minCheck

    member this.Validate (value : Pulumi.Provider.PropertyValue) =
        match value.TryGetArray () with 
        | false, _ -> ()
        | true, arr ->
            match this.MinItems with 
            | Some l when uint arr.Length < l -> 
                failwithf "Value has fewer than %d items" l
            | _ -> ()

            match this.MaxItems with 
            | Some l when uint arr.Length > l -> 
                failwithf "Value has more than %d items" l
            | _ -> ()


type NullConversion = {
    Description : string option
} with     
    member this.BuildTypeSpec() = 
        let schema = JsonObject()
        schema.Add("$ref", JsonValue.Create("pulumi.json#/Any"))
        schema, this.Description

    member this.BuildPropertySpec() =
        let schema = JsonObject()
        schema.Add("$ref", JsonValue.Create("pulumi.json#/Any"))
        this.Description |> Option.iter (fun desc -> schema.Add("description", desc))
        schema
        
    member this.Writer (value : Pulumi.Provider.PropertyValue) = 
        let raise  (typ : string) = failwithf "Invalid type expected null got %s" typ
        value.Match(
            (fun _ -> None),
            (fun _ -> raise "bool"),
            (fun _ -> raise "number"),
            (fun _ -> raise "string"),
            (fun _ -> raise "array"),
            (fun _ -> raise "object"),
            (fun _ -> raise "asset"),
            (fun _ -> raise "archive"),
            (fun secret -> this.Writer secret),
            (fun _ -> raise "resource"),            
            (fun output -> this.Writer output.Value),
            (fun _ -> raise "computed")
        )

    member this.Reader (value : JsonElement) = 
        if value.ValueKind = JsonValueKind.Null then
            Ok (Pulumi.Provider.PropertyValue.Null)
        else 
            errorf "Invalid JSON document expected null got %O" value.ValueKind

let writePrimitive (typ : PrimitiveType) (validation : PrimitiveValidation) (value : Pulumi.Provider.PropertyValue) = 
    let rec getNode (value : Pulumi.Provider.PropertyValue) =
        match typ with 
        | PrimitiveType.Boolean ->
            let raise  (typ : string) = failwithf "Value is \"%s\" but should be \"boolean\"" typ
            value.Match(
                (fun _ -> raise "null"),
                (fun b -> 
                    JsonValue.Create(b) 
                    :> JsonNode
                    |> Some
                ),
                (fun _ -> raise "number"),
                (fun _ -> raise "string"),
                (fun _ -> raise "array"),
                (fun _ -> raise "object"),
                (fun _ -> raise "asset"),
                (fun _ -> raise "archive"),
                (fun secret -> getNode secret),
                (fun _ -> raise "resource"),            
                (fun output -> getNode output.Value),
                (fun _ -> raise "computed")
            )
        | PrimitiveType.Integer -> 
            let raise  (typ : string) = failwithf "Value is \"%s\" but should be \"integer\"" typ
            value.Match(
                (fun _ -> raise "null"),
                (fun _ -> raise "bool"),
                (fun n -> 
                    match validation.Numeric.Validate (decimal n) with
                    | Some err -> failwith err
                    | None -> ()

                    if floor n <> n then raise "number"
                    JsonValue.Create(n) 
                    :> JsonNode
                    |> Some
                ),
                (fun _ -> raise "string"),
                (fun _ -> raise "array"),
                (fun _ -> raise "object"),
                (fun _ -> raise "asset"),
                (fun _ -> raise "archive"),
                (fun secret -> getNode secret),
                (fun _ -> raise "resource"),            
                (fun output -> getNode output.Value),
                (fun _ -> raise "computed")
            )
        | PrimitiveType.Number ->
            let raise  (typ : string) = failwithf "Value is \"%s\" but should be \"number\"" typ
            value.Match(
                (fun _ -> raise "null"),
                (fun _ -> raise "bool"),
                (fun n -> 
                    match validation.Numeric.Validate (decimal n) with
                    | Some err -> failwith err
                    | None -> ()

                    JsonValue.Create(n) 
                    :> JsonNode
                    |> Some
                ),
                (fun _ -> raise "string"),
                (fun _ -> raise "array"),
                (fun _ -> raise "object"),
                (fun _ -> raise "asset"),
                (fun _ -> raise "archive"),
                (fun secret -> getNode secret),
                (fun _ -> raise "resource"),            
                (fun output -> getNode output.Value),
                (fun _ -> raise "computed")
            )
        | PrimitiveType.String ->
            let raise  (typ : string) = failwithf "Value is \"%s\" but should be \"string\"" typ
            value.Match(
                (fun _ -> raise "null"),
                (fun _ -> raise "bool"),
                (fun n -> 
                    if floor n = n then raise "integer"
                    else raise "number"
                ),
                (fun s -> 
                    match validation.String.Validate s with
                    | Some err -> failwith err
                    | None -> ()

                    JsonValue.Create(s) 
                    :> JsonNode
                    |> Some
                ),
                (fun _ -> raise "array"),
                (fun _ -> raise "object"),
                (fun _ -> raise "asset"),
                (fun _ -> raise "archive"),
                (fun secret -> getNode secret),
                (fun _ -> raise "resource"),            
                (fun output -> getNode output.Value),
                (fun _ -> raise "computed")
            )
    let n = getNode value
    match validation.Validate n with 
    | Some err -> failwith err
    | None -> n

let readPrimitive (typ: PrimitiveType) (validation : PrimitiveValidation)  (value : JsonElement) =
    let valueTyp = 
        match value.ValueKind with 
        | JsonValueKind.Null -> "null"
        | JsonValueKind.True -> "boolean"
        | JsonValueKind.False -> "boolean"
        | JsonValueKind.Number -> 
            let isInteger, _ = value.TryGetInt64()
            if isInteger then "integer" else "number"
        | JsonValueKind.String -> "string"
        | JsonValueKind.Array -> "array"
        | JsonValueKind.Object -> "object"
        | _ -> failwith "Unexpected JsonValueKind"
        
    match validation.Validate (Json.More.JsonElementExtensions.AsNode value |> Option.ofObj) with 
    | Some err -> errorf "%s" err
    | None -> 

    match typ with 
    | PrimitiveType.Boolean ->
        if value.ValueKind = JsonValueKind.True then
            Ok (Pulumi.Provider.PropertyValue true)
        elif value.ValueKind = JsonValueKind.False then
            Ok (Pulumi.Provider.PropertyValue false)
        else 
            errorf "Value is \"%s\" but should be \"boolean\"" valueTyp
    | PrimitiveType.Integer ->
        if value.ValueKind = JsonValueKind.Number then
            match value.TryGetInt64() with
            | false, _ -> errorf "Value is \"number\" but should be \"integer\"" 
            | true, num ->
                match validation.Numeric.Validate (decimal num) with
                | Some err -> Error err
                | None -> Ok (float num)
                |> Result.map Pulumi.Provider.PropertyValue
        else 
            errorf "Value is \"%s\" but should be \"integer\"" valueTyp
    | PrimitiveType.Number ->
        if value.ValueKind = JsonValueKind.Number then
            let num = value.GetDouble()
            match validation.Numeric.Validate (decimal num) with
            | Some err -> Error err
            | None -> Ok num
            |> Result.map Pulumi.Provider.PropertyValue
        else 
            errorf "Value is \"%s\" but should be \"number\"" valueTyp
    | PrimitiveType.String ->
        if value.ValueKind = JsonValueKind.String then
            let str = value.GetString()
            match validation.String.Validate str with
            | Some err -> Error err
            | None -> Ok str
            |> Result.map Pulumi.Provider.PropertyValue
        else 
            errorf "Value is \"%s\" but should be \"string\"" valueTyp
            
type Annotations = {
    PrefixItems : int
    Items : bool
    Properties : Set<string>
    AdditionalProperties : Set<string>
} with
    static member Empty =
        {
            PrefixItems = 0
            Items = false
            Properties = Set.empty
            AdditionalProperties = Set.empty
        }

    member this.AddProperty p = { this with Properties = Set.add p this.Properties }

    member this.AddAdditionalProperty p = { this with AdditionalProperties = Set.add p this.AdditionalProperties }

    member this.Union (other : Annotations) =
        {
            PrefixItems = max this.PrefixItems other.PrefixItems
            Items = this.Items || other.Items
            Properties = Set.union this.Properties other.Properties
            AdditionalProperties = Set.union this.AdditionalProperties other.AdditionalProperties
        }

let rec writeAny (validation : PrimitiveValidation) (value : Pulumi.Provider.PropertyValue) =
    value.Match<JsonNode option>(
        (fun () -> None),
        (fun b -> JsonValue.Create(b) :> JsonNode |> Some),
        (fun n -> 
            match validation.Numeric.Validate (decimal n) with
            | Some err -> failwith err
            | None -> ()
            JsonValue.Create(n) :> JsonNode |> Some),
        (fun s ->
            match validation.String.Validate s with
            | Some err -> failwith err
            | None -> ()
            JsonValue.Create(s) :> JsonNode |> Some),
        (fun a -> 
            a
            |> Seq.map (fun i -> writeAny PrimitiveValidation.None i |> Option.toObj)
            |> Seq.toArray
            |> JsonArray
            :> JsonNode
            |> Some
        ),
        (fun o -> 
            o
            |> Seq.map (fun kv -> 
                KeyValuePair.Create(kv.Key, writeAny PrimitiveValidation.None kv.Value |> Option.toObj)
            )
            |> Seq.toArray
            |> JsonObject
            :> JsonNode
            |> Some
        ),
        (fun _ -> failwith "not implemented"),
        (fun _ -> failwith "not implemented"),
        (fun secret -> writeAny validation secret),
        (fun _ -> failwith "not implemented"),   
        (fun output -> writeAny validation output.Value),
        (fun _ ->failwith "not implemented")
    )

let rec readAny (validation : PrimitiveValidation) (value : JsonElement) =
        if value.ValueKind = JsonValueKind.Null then
            Ok (Pulumi.Provider.PropertyValue.Null)
        elif value.ValueKind = JsonValueKind.False then 
            Ok (Pulumi.Provider.PropertyValue false)
        elif value.ValueKind = JsonValueKind.True then 
            Ok (Pulumi.Provider.PropertyValue true)
        elif value.ValueKind = JsonValueKind.Number then 
            Ok (Pulumi.Provider.PropertyValue(value.GetDouble()))
        elif value.ValueKind = JsonValueKind.String then 
            Ok (Pulumi.Provider.PropertyValue(value.GetString()))
        elif value.ValueKind = JsonValueKind.Array then 
            value.EnumerateArray()
            |> Seq.map (fun item -> 
                readAny PrimitiveValidation.None item
                |> Result.map fst)
            |> Seq.toList
            |> okList
            |> Result.mapError (fun errs -> String.concat ", " errs)
            |> Result.map (fun items -> 
                items 
                |> ImmutableArray.CreateRange
                |> Pulumi.Provider.PropertyValue)
        elif value.ValueKind = JsonValueKind.Object then 
            value.EnumerateObject()
            |> Seq.map (fun item -> 
                match readAny PrimitiveValidation.None item.Value with 
                | Ok (v, _) -> Ok (KeyValuePair.Create(item.Name, v))
                | Error err -> Error err)
            |> Seq.toList
            |> okList
            |> Result.mapError (fun errs -> String.concat ", " errs)
            |> Result.map (fun items -> 
                items
                |> ImmutableDictionary.CreateRange
                |> Pulumi.Provider.PropertyValue)
        else 
            Error (sprintf "unexpected JsonValueKind: %O" value.ValueKind)
        |> Result.map (fun r -> r, Annotations.Empty)

type AnyConversion = {
    Description : string option
    PrimitiveValidation : PrimitiveValidation
} with
    member this.BuildTypeSpec () = 
        let schema = JsonObject()
        schema.Add("$ref", JsonValue.Create("pulumi.json#/Any"))
        schema, this.Description

    member this.BuildPropertySpec () = 
        let schema = JsonObject()
        schema.Add("$ref", JsonValue.Create("pulumi.json#/Any"))
        this.Description |> Option.iter (fun desc -> schema.Add("description", desc))
        schema
        
    member this.Writer (value : Pulumi.Provider.PropertyValue) = writeAny this.PrimitiveValidation value
        
    member this.Reader (value : JsonElement) =  readAny this.PrimitiveValidation value

type PrimitiveConversion = {
    Description : string option
    Const : Choice<unit, bool, double, string>
    Type : PrimitiveType
    Validation : PrimitiveValidation
} with
    member this.BuildTypeSpec() = 
        let schema = JsonObject()
        schema.Add("type", this.Type.JsonValue)
        schema, this.Description

    member this.BuildPropertySpec() =
        let schema = JsonObject()
        schema.Add("type", this.Type.JsonValue)
        this.Description |> Option.iter (fun desc -> schema.Add("description", desc))
        match this.Const with 
        | Choice1Of4 () -> ()
        | Choice2Of4 b -> schema.Add("const", b)
        | Choice3Of4 n -> schema.Add("const", n)
        | Choice4Of4 s -> schema.Add("const", s)
        schema

    member this.Writer (value : Pulumi.Provider.PropertyValue) = 
        writePrimitive this.Type this.Validation value

    member this.Reader (value : JsonElement) = 
        readPrimitive this.Type this.Validation value

type UnionConversion = {
    Description : string option
    BooleanConversion : PrimitiveConversion option
    IntegerConversion : PrimitiveConversion option
    NumberConversion : PrimitiveConversion option
    StringConversion : PrimitiveConversion option
} with
    static member OfPrimitive (conversion : PrimitiveConversion) =
        match conversion.Type with
        | PrimitiveType.Boolean -> { Description = None; BooleanConversion = Some conversion; IntegerConversion = None; NumberConversion = None; StringConversion = None }
        | PrimitiveType.Integer -> { Description = None; BooleanConversion = None; IntegerConversion = Some conversion; NumberConversion = None; StringConversion = None }
        | PrimitiveType.Number -> { Description = None; BooleanConversion = None; IntegerConversion = None; NumberConversion = Some conversion; StringConversion = None }
        | PrimitiveType.String -> { Description = None; BooleanConversion = None; IntegerConversion = None; NumberConversion = None; StringConversion = Some conversion }

    member this.TryMerge (other : UnionConversion) =
        let unique a b = 
            match a, b with 
            | None, None -> Result.Ok None
            | Some a, None -> Result.Ok (Some a)
            | None, Some b -> Result.Ok (Some b)
            | _, _ -> Result.Error ()
        
        let boolean = unique this.BooleanConversion other.BooleanConversion 
        let integer = unique this.IntegerConversion other.IntegerConversion
        let number = unique this.NumberConversion other.NumberConversion 
        let str = unique this.StringConversion other.StringConversion

        match boolean, integer, number, str with 
        | Ok b, Ok i, Ok n, Ok s -> 
            Some { Description = this.Description; BooleanConversion = b; IntegerConversion = i; NumberConversion = n; StringConversion = s }
        | _ -> None

    member this.BuildTypeSpec () =
        let schema = JsonObject()

        let oneof = JsonArray()
        [this.BooleanConversion; this.IntegerConversion; this.NumberConversion; this.StringConversion]
        |> List.iter (function 
            | None -> ()
            | Some conversion -> 
                let spec, _ = conversion.BuildTypeSpec()
                oneof.Add(spec))
        schema.Add("oneOf", oneof)

        schema, this.Description

    member this.BuildPropertySpec () =
        let schema = JsonObject()

        let oneof = JsonArray()
        [this.BooleanConversion; this.IntegerConversion; this.NumberConversion; this.StringConversion]
        |> List.iter (function 
            | None -> ()
            | Some conversion -> 
                let spec, _ = conversion.BuildTypeSpec()
                oneof.Add(spec))
        schema.Add("oneOf", oneof)
                
        this.Description |> Option.iter (fun desc -> schema.Add("description", desc))
        schema

    member private this.expectedTypes = 
        ["integer", this.IntegerConversion; "number", this.NumberConversion; "string", this.StringConversion; "boolean", this.BooleanConversion]
          |> List.choose (fun (k, opt) -> match opt with | Some _ -> Some k | None -> None)
          |> String.concat " or "      

    member this.Writer (value : Pulumi.Provider.PropertyValue) = 
        let raise () = failwithf "Invalid type expected %s got %O" this.expectedTypes value.Type
        value.Match(
            (fun _ -> raise ()),
            (fun _ -> 
                match this.BooleanConversion with 
                | None -> raise ()
                | Some conversion -> conversion.Writer value
            ),
            (fun n -> 
                match this.IntegerConversion with 
                | Some conversion when n = floor n -> conversion.Writer value
                | _ -> 
                    match this.NumberConversion with 
                    | Some conversion -> conversion.Writer value
                    | None -> raise ()
            ),
            (fun _ -> 
                match this.StringConversion with 
                | None -> raise ()
                | Some conversion -> conversion.Writer value
            ),
            (fun _ -> raise ()),
            (fun _ -> raise ()),
            (fun _ -> raise ()),
            (fun _ -> raise ()),
            (fun secret -> this.Writer secret),
            (fun _ -> raise ()),
            (fun output -> this.Writer output.Value),
            (fun _ -> raise ())
        )

    member this.Reader (value : JsonElement) = 
        if (value.ValueKind = JsonValueKind.True || value.ValueKind = JsonValueKind.False) && this.BooleanConversion.IsSome then
            this.BooleanConversion.Value.Reader value
        elif value.ValueKind = JsonValueKind.Number && (this.NumberConversion.IsSome || this.IntegerConversion.IsSome) then
            match tryGetNumeric value with
            | Some (Choice1Of2 i) -> 
                if this.IntegerConversion.IsSome then
                    this.IntegerConversion.Value.Reader value
                else 
                    this.NumberConversion.Value.Reader value
            | Some (Choice2Of2 n) when this.NumberConversion.IsSome -> this.NumberConversion.Value.Reader value
            | Some (Choice2Of2 _) -> failwithf "Invalid JSON document expected %s got %O" this.expectedTypes value.ValueKind
            | None -> failwith "Expected tryGetNumeric to return a value for a number"            
        elif value.ValueKind = JsonValueKind.String && this.StringConversion.IsSome then
            this.StringConversion.Value.Reader value
        else 
            failwithf "Invalid JSON document expected %s got %O" this.expectedTypes value.ValueKind

type EnumConversion = {
    Path : Json.Pointer.JsonPointer
    Title : string option
    Description : string option
    Type : PrimitiveType
    Values : JsonNode list
} with        
    member this.BuildComplexTypeSpec() =
        let schema = JsonObject()
        schema.Add("type", this.Type.JsonValue)
        let values = 
            this.Values
            |> Seq.map (fun v -> 
                let value = Json.More.JsonNodeExtensions.Copy(v)
                JsonObject([KeyValuePair.Create("value", value)]) :> JsonNode
            )
            |> Seq.toArray
            |> JsonArray
        schema.Add("enum", values)
        this.Description |> Option.iter (fun desc -> schema.Add("description", desc))
        schema

    member this.Writer (value : Pulumi.Provider.PropertyValue) = 
        let value = writePrimitive this.Type PrimitiveValidation.None value
        let isMatch = 
            this.Values
            |> Seq.exists (fun node -> 
                Json.More.JsonNodeEqualityComparer.Instance.Equals(
                    node,
                    Option.toObj value)
            )
        if isMatch then value
        else failwith "Expected value to match one of the values specified by the enum"

    member this.Reader (value : JsonElement) = 
        let isMatch = 
            this.Values
            |> Seq.exists (fun node -> 
                Json.More.JsonNodeEqualityComparer.Instance.Equals(
                    node,
                    Json.More.JsonElementExtensions.AsNode(value))
            )

        if isMatch then readPrimitive this.Type PrimitiveValidation.None value
        else errorf "Expected value to match one of the values specified by the enum"
     
type ArrayConversion = {
    Description : string option
    Items : Conversion option
    Validation : ArrayValidation
} with
    member this.BuildTypeSpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) =
        let schema = JsonObject()
        schema.Add("type", JsonValue.Create("array"))
        let items = Option.defaultValue Conversion.True this.Items
        let itemsSpec, desc = items.BuildTypeSpec packageName names
        schema.Add("items", itemsSpec)
        schema, this.Description

    member this.BuildPropertySpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) = 
        let schema, desc = this.BuildTypeSpec packageName names
        desc |> Option.iter (fun desc -> schema.Add("description", desc))
        schema
        
    member this.Writer (value : Pulumi.Provider.PropertyValue) = 
        this.Validation.Validate value

        let maybeArr = 
            value.TryUnwrap ()
            |> optionOfTry
            |> Option.bind (fun v -> v.TryGetArray() |> optionOfTry)

        let itemWriter = 
            match this.Items with 
            | Some items -> items.Writer
            | None -> writeAny PrimitiveValidation.None

        match maybeArr with 
        | None -> failwithf "Invalid type expected array got %O" value.Type
        | Some arr ->
            arr
            |> Seq.map (fun item ->
                itemWriter item
                |> Option.toObj
            )
            |> Seq.toArray
            |> JsonArray
            :> JsonNode
            |> Some

    member this.Reader (value : JsonElement) =
        if value.ValueKind = JsonValueKind.Array then
            match this.Validation.Validate value with 
            | Some err -> Error err
            | None ->

            let itemReader item = 
                match this.Items with 
                | Some items -> items.Reader item
                | None -> readAny PrimitiveValidation.None item
                |> Result.map fst

            value.EnumerateArray()
            |> Seq.map (fun item -> itemReader item)
            |> Seq.toList
            |> okList
            |> Result.mapError (fun errs -> String.concat ", " errs)
            |> Result.map (fun items ->
                let result = 
                    items
                    |> ImmutableArray.CreateRange
                    |> Pulumi.Provider.PropertyValue
                match this.Items with 
                | Some _ -> result, { Annotations.Empty with Items = true }
                | None -> result, Annotations.Empty
            )
        else 
            errorf "Invalid JSON document expected array got %O" value.ValueKind

    member this.CollectComplexTypes path refs : ImmutableDictionary<ComplexTypeSpec, Set<string list>> =
        match this.Items with
        | Some items -> items.CollectComplexTypes ("items" :: path) refs
        | None -> ImmutableDictionary.Empty

and MapConversion = {
    Description : string option
    AdditionalProperties : Conversion option
} with
    member this.BuildTypeSpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) = 
        let schema = JsonObject()
        schema.Add("type", JsonValue.Create("object"))
        let aps = Option.defaultValue Conversion.True this.AdditionalProperties
        let additionalProperties, desc = aps.BuildTypeSpec packageName names
        schema.Add("additionalProperties", additionalProperties)
        schema, this.Description

    member this.BuildPropertySpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) = 
        let schema = JsonObject()
        schema.Add("type", JsonValue.Create("object"))
        let aps = Option.defaultValue Conversion.True this.AdditionalProperties
        let additionalProperties, desc = aps.BuildTypeSpec packageName names
        schema.Add("additionalProperties", additionalProperties)
        this.Description |> Option.iter (fun desc -> schema.Add("description", desc))
        schema
        
    member this.Writer (value : Pulumi.Provider.PropertyValue) =
        let maybeObj = 
            value.TryUnwrap ()
            |> optionOfTry
            |> Option.bind (fun v -> v.TryGetObject() |> optionOfTry)
                
        match maybeObj with 
        | None -> failwithf "Invalid type expected object got %O" value.Type
        | Some obj ->
            let aps = Option.defaultValue Conversion.True this.AdditionalProperties

            obj
            |> Seq.map (fun kv ->
                let node = aps.Writer kv.Value
                match node with
                | Some node -> KeyValuePair.Create(kv.Key, node)
                | None -> KeyValuePair.Create(kv.Key, null)
            )
            |> JsonObject
            :> JsonNode
            |> Some

    member this.Reader (value : JsonElement) = 
        if value.ValueKind = JsonValueKind.Object then
            let aps = Option.defaultValue Conversion.True this.AdditionalProperties

            value.EnumerateObject()
            |> Seq.map (fun kv -> 
                match aps.Reader kv.Value with 
                | Ok (ok, _) -> Ok (KeyValuePair.Create(kv.Name, ok))
                | Error err -> Error err)
            |> Seq.toList
            |> okList
            |> Result.mapError (fun errs -> String.concat ", " errs)
            |> Result.map (fun items -> 
                let result = 
                    items
                    |> ImmutableDictionary.CreateRange
                    |> Pulumi.Provider.PropertyValue

                let annotations = 
                    items 
                    |> List.fold (fun (a : Annotations) i -> a.AddAdditionalProperty i.Key) Annotations.Empty

                result, annotations
            )
        else 
            errorf "Invalid JSON document expected object got %O" value.ValueKind

    member this.CollectComplexTypes path refs : ImmutableDictionary<ComplexTypeSpec, Set<string list>> =
        let aps = Option.defaultValue Conversion.True this.AdditionalProperties
        aps.CollectComplexTypes ("additionalProperties" :: path) refs

and [<RequireQualifiedAccess>] TypeSpec = 
    | Any of AnyConversion
    | Null of NullConversion
    | Primitive of PrimitiveConversion
    | Array of ArrayConversion
    | Map of MapConversion
    | Union of UnionConversion

    member this.BuildTypeSpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) = 
        match this with 
        | Any c -> c.BuildTypeSpec()
        | Null c -> c.BuildTypeSpec()
        | Primitive c -> c.BuildTypeSpec()
        | Array c -> c.BuildTypeSpec packageName names
        | Map c -> c.BuildTypeSpec packageName names
        | Union c -> c.BuildTypeSpec()

    member this.BuildPropertySpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) = 
        match this with 
        | Any c -> c.BuildPropertySpec()
        | Null c -> c.BuildPropertySpec()
        | Primitive c -> c.BuildPropertySpec()
        | Array c -> c.BuildPropertySpec packageName names
        | Map c -> c.BuildPropertySpec packageName names
        | Union c -> c.BuildPropertySpec()
        
    member this.Writer (value : Pulumi.Provider.PropertyValue) = 
        match this with 
        | Any c -> c.Writer value
        | Null c -> c.Writer value
        | Primitive c -> c.Writer value
        | Array c -> c.Writer value
        | Map c -> c.Writer value        
        | Union c -> c.Writer value
        
    member this.Reader (value : JsonElement) = 
        match this with 
        | Any c -> c.Reader value
        | Null c -> c.Reader value |> Result.map (fun r -> r, Annotations.Empty)
        | Primitive c -> c.Reader value |> Result.map (fun r -> r, Annotations.Empty)
        | Array c -> c.Reader value
        | Map c -> c.Reader value
        | Union c -> c.Reader value|> Result.map (fun r -> r, Annotations.Empty)

    member this.CollectComplexTypes path refs : ImmutableDictionary<ComplexTypeSpec, Set<string list>> =
        match this with 
        | Any _ -> ImmutableDictionary.Empty
        | Null _ -> ImmutableDictionary.Empty
        | Primitive _ -> ImmutableDictionary.Empty
        | Array c -> c.CollectComplexTypes path refs 
        | Map c -> c.CollectComplexTypes path refs
        | Union _ -> ImmutableDictionary.Empty

and DiscriminateUnionConversion = {
    Path : Json.Pointer.JsonPointer
    Title : string option
    Description : string option
    Choices : Conversion list
} with 

    member this.BuildComplexTypeSpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) =
        let schema = JsonObject()
        schema.Add("type", JsonValue.Create("object"))
        this.Description |> Option.iter (fun desc -> schema.Add("description", desc))

        let propertiesSchema = JsonObject()
        schema.Add("properties", propertiesSchema)
        this.Choices
        |> Seq.iteri (fun i choice -> 
            let name = sprintf "choice%dOf%d" (i+1) this.Choices.Length
            propertiesSchema.Add(name, choice.BuildPropertySpec packageName names)
        )

        schema

    member this.Writer (value : Pulumi.Provider.PropertyValue) =
        let maybeObj = 
            value.TryUnwrap ()
            |> optionOfTry
            |> Option.bind (fun v -> v.TryGetObject() |> optionOfTry)
                
        match maybeObj with 
        | None -> failwithf "Invalid type expected object got %O" value.Type
        | Some obj ->
            // Only 1 of our fields should be set, we can just write that field out directly
            if obj.Count <> 1 then
                failwith "Multiple properties were set for oneOf"
            else 
                let choiceRegex = System.Text.RegularExpressions.Regex("^choice(\d+)Of(\d)$")
                let item = Seq.head obj
                let reMatch = choiceRegex.Match item.Key
                if not reMatch.Success then
                    failwithf "unexpected property '%s'" item.Key

                match List.tryItem (Int32.Parse(reMatch.Groups[1].Value) - 1) this.Choices with 
                | None -> failwithf "unexpected property '%s'" item.Key
                | Some conversion -> conversion.Writer item.Value

    member this.Reader (value : JsonElement) =
        this.Choices
        |> List.mapi (fun i choice -> i, choice)
        |> List.choose (fun (i, choice) ->
            match choice.Reader value with 
            | Ok (v, a) -> Some (KeyValuePair.Create(sprintf "choice%dOf%d" (i+1) this.Choices.Length, v), a)
            | Error e -> None
        )
        |> function 
           | [(result, a)] -> Ok (Pulumi.Provider.PropertyValue(ImmutableDictionary.CreateRange [result]), a)
           | results -> errorf "Expected 1 matching subschema but found %d" results.Length

    member this.CollectComplexTypes path refs : ImmutableDictionary<ComplexTypeSpec, Set<string list>> =
        this.Choices
        |> Seq.mapi (fun i c -> i, c)
        |> Seq.fold (fun types (i, choice) -> 
            let seg = sprintf "choice%dOf%d" (i+1) this.Choices.Length            
            Conversion.UnionComplexTypes (choice.CollectComplexTypes (seg :: path) refs) types
        ) ImmutableDictionary.Empty

and ObjectConversion = {
    Path : Json.Pointer.JsonPointer
    Title : string option
    Description : string option
    Properties : Map<string, string * Conversion>
    // There are three states for AdditionalProperties:
    // 1. Explictly set to a scheam
    // 2. Unset but should be added as an object property
    // 3. Unset and so should pass validation, but shouldn't be added as a property
    // Mode 3 is so we can have nested objects without having additionalProperties on all of them
    AdditionalProperties : Choice<unit, Conversion option>
    Choices : Conversion list
    Required : Set<string>
} with 

    member this.BuildComplexTypeSpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) =
        let schema = JsonObject()
        schema.Add("type", JsonValue.Create("object"))
        this.Description |> Option.iter (fun desc -> schema.Add("description", desc))

        if not this.Required.IsEmpty then
            let requiredArray =
                this.Required
                |> Seq.map (fun str -> JsonValue.Create(str) :> JsonNode)
                |> Seq.toArray
                |> JsonArray
            schema.Add("required", requiredArray)

        let propertiesSchema = JsonObject()
        schema.Add("properties", propertiesSchema)
        for kv in this.Properties do
            let (_, prop) = kv.Value
            if not prop.IsFalseSchema then
                propertiesSchema.Add(kv.Key, prop.BuildPropertySpec packageName names)
            
        match this.AdditionalProperties with 
        | Choice1Of2 () -> ()
        | Choice2Of2 (Some aps) when aps.IsFalseSchema -> ()
        | Choice2Of2 aps ->
            let aps = Option.defaultValue Conversion.True aps 

            // additionalProperties is always just a map
            let additionalPropertiesSchema = JsonObject()
            additionalPropertiesSchema.Add("type", "object")
            let items, _ = aps.BuildTypeSpec packageName names
            additionalPropertiesSchema.Add("additionalProperties", items)
            propertiesSchema.Add("additionalProperties", additionalPropertiesSchema)

        this.Choices
        |> Seq.iteri (fun i choice -> 
            let name = sprintf "choice%dOf%d" (i+1) this.Choices.Length
            propertiesSchema.Add(name, choice.BuildPropertySpec packageName names)
        )

        schema

    member this.Writer (value : Pulumi.Provider.PropertyValue) =
        let maybeObj = 
            value.TryUnwrap ()
            |> optionOfTry
            |> Option.bind (fun v -> v.TryGetObject() |> optionOfTry)
                
        match maybeObj with 
        | None -> failwithf "Invalid type expected object got %O" value.Type
        | Some obj ->
            // Check all the required keys are present
            for key in this.Required do 
                if not (obj.ContainsKey key) then
                    failwithf "property '%s' is required" key
                    
            let additionalProperties = 
                match this.AdditionalProperties with 
                | Choice1Of2 () -> Conversion.True
                | Choice2Of2 aps -> Option.defaultValue Conversion.True aps

            obj
            |> Seq.collect (fun kv ->
                if kv.Key = "additionalProperties" then
                    // This _should_ be an object
                    match kv.Value.TryGetObject() with
                    | false, _ -> failwithf "Invalid type expected object got %O" kv.Value
                    | true, additionalObj ->
                        additionalObj
                        |> Seq.map (fun kv ->
                            // Check if this key is in the properties map, if it is it shouldn't be here
                            match this.Properties.TryFind kv.Key with 
                            | Some (_, _) -> failwith "All values fail against the false schema"
                            | None ->
                                let node = additionalProperties.Writer kv.Value
                                match node with 
                                | Some node -> KeyValuePair.Create(kv.Key, node)
                                | None -> KeyValuePair.Create(kv.Key, null)
                        )
                else 
                    match this.Properties.TryFind kv.Key with 
                    | Some (jsonName, conversion) ->
                        let node = conversion.Writer kv.Value
                        match node with 
                        | Some node -> [KeyValuePair.Create(jsonName, node)]
                        | None -> [KeyValuePair.Create(jsonName, null)]
                    | None -> failwithf "unexpected property '%s'" kv.Key
            )
            |> JsonObject
            :> JsonNode
            |> Some

    member this.Reader (value : JsonElement) =
        if value.ValueKind = JsonValueKind.Object then            
            let tryFindName (name : string) = 
                this.Properties
                |> Map.tryPick (fun pulumiName (jsonName, conversion) -> 
                    if jsonName = name then Some (pulumiName, conversion)
                    else None
                )

            let obj = 
                value.EnumerateObject()
                |> Seq.map (fun p -> p.Name, p.Value)
                |> Map.ofSeq
                
            // Check all the required keys are present
            let requiredErr =
                this.Required
                |> Seq.tryPick (fun key ->
                    if obj.ContainsKey key |> not then
                        Some (errorf "property '%s' is required" key)
                    else None
                ) 
                |> Option.defaultValue (Ok ())

            match requiredErr with 
            | Error err -> Error err
            | Ok () ->

            let choice = 
                match this.Choices with
                | [] -> Ok None 
                | choices -> 
                    choices
                    |> List.mapi (fun i choice -> i, choice)
                    |> List.choose (fun (i, choice) ->
                        match choice.Reader value with 
                        | Ok (v, annotations) -> Some (KeyValuePair.Create(sprintf "choice%dOf%d" (i+1) this.Choices.Length, v), annotations)
                        | Error e -> None
                    )
                    |> function 
                       | [result] -> Ok (Some result)
                       | results -> errorf "Expected 1 matching subschema but found %d" results.Length
                    
            match choice with 
            | Error err -> Error err
            | Ok choice ->
                    
            let additionalProperties, addProperty = 
                match this.AdditionalProperties with 
                | Choice1Of2 () -> Conversion.True, false
                | Choice2Of2 aps -> Option.defaultValue Conversion.True aps, true

            let propertiesAdditionalProperties =
                obj
                |> Seq.fold (fun props kv ->
                    match props with 
                    | Error err -> Error err
                    | Ok (propsSoFar, addPropsSoFar) ->
                        match tryFindName kv.Key with
                        | Some (pulumiName, conversion) ->
                            match conversion.Reader kv.Value with 
                            | Ok (ok, _) -> Ok (KeyValuePair.Create(pulumiName, ok) :: propsSoFar, addPropsSoFar)
                            | Error err -> Error err
                        | None ->
                            let inChoice = 
                                match choice with 
                                | None -> false
                                | Some (_, annotations) -> 
                                    annotations.Properties.Contains kv.Key ||
                                    annotations.AdditionalProperties.Contains kv.Key

                            if inChoice then Ok (propsSoFar, addPropsSoFar)
                            else
                                match additionalProperties.Reader kv.Value with  
                                | Ok (ok, _) -> Ok (propsSoFar, KeyValuePair.Create(kv.Key, ok) :: addPropsSoFar)
                                | Error err -> Error err
                ) (Ok ([], []))

            match propertiesAdditionalProperties with 
            | Error err -> Error err
            | Ok (properties, additionalProperties) ->

            let annotations = 
                properties
                |> List.fold (fun (a : Annotations) p -> a.AddProperty p.Key) Annotations.Empty

            // If we have any additionalProperties add them to the properties
            let properties, annotations = 
                match additionalProperties, addProperty with
                | [], _ 
                | _, false -> properties, annotations
                | additionalProperties, true ->
                    let arr = Pulumi.Provider.PropertyValue(ImmutableDictionary.CreateRange additionalProperties)
                    let annotations = 
                        additionalProperties
                        |> List.fold (fun (a : Annotations) p -> a.AddAdditionalProperty p.Key) annotations
                    KeyValuePair.Create("additionalProperties", arr) :: properties, annotations
            
            // Add the choice to properties
            let propertiesAnnotations = 
                match choice with
                | None -> Ok (properties, annotations)
                | Some (choice, choiceAnnotations) -> 
                    Ok (choice :: properties, annotations.Union choiceAnnotations)

            propertiesAnnotations
            |> Result.map (fun (properties, annotations) ->
                properties |> ImmutableDictionary.CreateRange |> Pulumi.Provider.PropertyValue, annotations)
        else 
            failwithf "Invalid JSON document expected object got %O" value.ValueKind

    member this.CollectComplexTypes path refs : ImmutableDictionary<ComplexTypeSpec, Set<string list>> =
        let complexTypes = 
            this.Properties
            |> Seq.fold (fun types kv -> 
                let (_, prop) = kv.Value
                let path = kv.Key :: path
                Conversion.UnionComplexTypes (prop.CollectComplexTypes path refs) types
            ) ImmutableDictionary.Empty
        let additionalTypes =
            match this.AdditionalProperties with
            | Choice2Of2 (Some aps) -> 
                let path = "additionalProperties" :: path
                Conversion.UnionComplexTypes (aps.CollectComplexTypes path refs) complexTypes
            | _ -> complexTypes
            
        this.Choices
        |> Seq.mapi (fun i c -> i, c)
        |> Seq.fold (fun types (i, kv) ->
            let name = sprintf "choice%dOf%d" (i+1) this.Choices.Length
            let path = name :: path
            Conversion.UnionComplexTypes (kv.CollectComplexTypes path refs) types
        ) additionalTypes

and TupleConversion = {
    Path : Json.Pointer.JsonPointer
    Title : string option
    Description : string option
    PrefixItems : Conversion list
    AdditionalItems : Conversion option
    Validation : ArrayValidation
} with 

    member this.BuildComplexTypeSpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) =
        let schema = JsonObject()
        schema.Add("type", JsonValue.Create("object"))
        this.Description |> Option.iter (fun desc -> schema.Add("description", desc))
        let propertiesSchema = JsonObject()
        schema.Add("properties", propertiesSchema)
        
        this.PrefixItems
        |> Seq.iteri (fun i s -> 
            let key = sprintf "item%d" (i+1)
            propertiesSchema.Add(key, s.BuildPropertySpec packageName names)
        )

        match this.Validation.MinItems with
        | None -> ()
        | Some minItems ->
            // If at least X items are required then we know item1 to item(X+1) are required
            let required = JsonArray()
            let requiredCount = min (int minItems) this.PrefixItems.Length
            for i = 1 to requiredCount do
                required.Add(sprintf "item%d" i)
            schema.Add("required", required)

        match this.AdditionalItems with
        | Some ais when ais.IsFalseSchema -> ()
            // AdditionalItems is false so no need for the extra property
        | ais ->
            // AdditionalItems is either unset (behaves like true) or set to something
            let ais = Option.defaultValue Conversion.True ais

            // additionalItems is always just an array
            let additionalItemsSchema = JsonObject()
            additionalItemsSchema.Add("type", "array")
            let items, _ = ais.BuildTypeSpec packageName names
            additionalItemsSchema.Add("items", items)
            propertiesSchema.Add("additionalItems", additionalItemsSchema)

        schema

    member this.Writer (value : Pulumi.Provider.PropertyValue) =
        let maybeObj = 
            value.TryUnwrap ()
            |> optionOfTry
            |> Option.bind (fun v -> v.TryGetObject() |> optionOfTry)
                
        match maybeObj with 
        | None -> failwithf "Invalid type expected object got %O" value.Type
        | Some obj ->
            let keyRegex = System.Text.RegularExpressions.Regex("^item(\d+)$")

            // If minItems is set then some of the items are required
            match this.Validation.MinItems with
            | None -> ()
            | Some minItems ->
                let requiredCount = min (int minItems) this.PrefixItems.Length
                for i = 1 to requiredCount do
                    let key = sprintf "item%d" i
                    if not (obj.ContainsKey key) then
                        failwithf "property '%s' is required" key

            // First write out an array of the prefix items
            let items, rest = 
                obj 
                |> Seq.fold (fun (items, rest) property ->
                    match this.AdditionalItems, property.Key, rest with
                    | None, "rest", _ -> failwith "unexpected property key 'rest'"
                    | Some ais , "rest", [||] ->
                        let maybeArr =
                            property.Value.TryUnwrap()
                            |> optionOfTry
                            |> Option.bind (fun v -> v.TryGetArray() |> optionOfTry)

                        match maybeArr with 
                        | None -> failwithf "Invalid type expected array got %O" value.Type
                        | Some arr -> (items, arr |> Seq.map ais.Writer |> Seq.toArray)
                    | Some _, "rest", _ -> failwith "unexpected duplicate property 'rest'"
                    | _, key, rest ->
                        let reMatch = keyRegex.Match key
                        if not reMatch.Success then
                            failwithf "unexpected property key '%s'" key

                        let index = System.Int32.Parse(reMatch.Groups[1].Value) - 1
                        match List.tryItem index this.PrefixItems with 
                        | None -> failwithf "unexpected property key '%s'" key
                        | Some item -> (Map.add index (item.Writer property.Value) items, rest)
                ) (Map.empty, [||])

            Array.init (items.Count + rest.Length) (fun i ->
                match Map.tryFind i items with 
                | Some item -> item
                | None -> Array.item (i - items.Count) rest
                |> Option.toObj
            )
            |> JsonArray
            :> JsonNode
            |> Some

    member this.Reader (value : JsonElement) =
        if value.ValueKind = JsonValueKind.Array then
            match this.Validation.Validate value with 
            | Some err -> Error err
            | None ->

            let propertiesRest =
                value.EnumerateArray()
                |> Seq.mapi (fun i item -> (i, item))
                |> Seq.fold (fun propertiesRest (i, item) ->
                    match propertiesRest with 
                    | Error err -> Error err
                    | Ok (properties, rest) ->
                        match List.tryItem i this.PrefixItems, this.AdditionalItems with 
                        | None, None ->                             
                            match readAny PrimitiveValidation.None item with 
                            | Ok (ok, _) -> Ok (properties, ok :: rest)
                            | Error err -> Error err                            
                        | None, Some ais -> 
                            match ais.Reader item with 
                            | Ok (ok, _) -> Ok (properties, ok :: rest)
                            | Error err -> Error err
                        | Some property, _ -> 
                            match property.Reader item with 
                            | Ok (ok, _) -> Ok (Map.add (i+1) ok properties, rest)
                            | Error err -> Error err
                ) (Ok (Map.empty, []))

            match propertiesRest with
            | Error err -> Error err
            | Ok (properties, rest) ->
                
            // If we have any additionalItems we'll add them to the properties
            let rest = 
                match this.AdditionalItems, rest with
                | Some ais, rest when ais.IsFalseSchema ->
                    match rest with 
                    | [] -> None
                    | _ -> failwith "This should be unreachable"
                | _, rest -> 
                    let arr = rest |> List.rev |> ImmutableArray.CreateRange
                    Some (KeyValuePair.Create("rest", Pulumi.Provider.PropertyValue(arr)))

            let properties = 
                properties
                |> Seq.map (fun kv -> 
                    KeyValuePair.Create(sprintf "item%d" kv.Key, kv.Value)
                )
                |> match rest with | None -> id | Some rest -> Seq.append [rest]

            properties
            |> ImmutableDictionary.CreateRange
            |> Pulumi.Provider.PropertyValue
            |> Ok
        else 
            errorf "Invalid JSON document expected array got %O" value.ValueKind
        |> Result.map (fun r -> r, Annotations.Empty)

    member this.CollectComplexTypes path refs : ImmutableDictionary<ComplexTypeSpec, Set<string list>> =
        let complexTypes = 
            this.PrefixItems
            |> Seq.fold (fun types item -> 
                Conversion.UnionComplexTypes (item.CollectComplexTypes ("prefixItems" :: path) refs) types
            ) ImmutableDictionary.Empty
        match this.AdditionalItems with
        | None -> complexTypes
        | Some ais -> Conversion.UnionComplexTypes (ais.CollectComplexTypes ("items" :: path) refs) complexTypes

and [<RequireQualifiedAccess>] ComplexTypeSpec =
    | Enum of EnumConversion
    | Object of ObjectConversion
    | DU of DiscriminateUnionConversion
    | Tuple of TupleConversion
    
    member this.Path = 
        match this with 
        | ComplexTypeSpec.Enum spec -> spec.Path
        | ComplexTypeSpec.Object spec -> spec.Path
        | ComplexTypeSpec.Tuple spec -> spec.Path
        | ComplexTypeSpec.DU spec -> spec.Path

    member this.Title = 
        match this with 
        | ComplexTypeSpec.Enum spec -> spec.Title
        | ComplexTypeSpec.Object spec -> spec.Title
        | ComplexTypeSpec.Tuple spec -> spec.Title
        | ComplexTypeSpec.DU spec -> spec.Title
    
    member this.BuildComplexTypeSpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) =
        match this with 
        | ComplexTypeSpec.Enum spec -> spec.BuildComplexTypeSpec()
        | ComplexTypeSpec.Object spec -> spec.BuildComplexTypeSpec packageName names
        | ComplexTypeSpec.Tuple spec -> spec.BuildComplexTypeSpec packageName names
        | ComplexTypeSpec.DU spec -> spec.BuildComplexTypeSpec packageName names

    member this.Writer (value : Pulumi.Provider.PropertyValue) =
        match this with 
        | ComplexTypeSpec.Enum spec -> spec.Writer value
        | ComplexTypeSpec.Object spec -> spec.Writer value
        | ComplexTypeSpec.Tuple spec -> spec.Writer value
        | ComplexTypeSpec.DU spec -> spec.Writer value

    member this.Reader (value : JsonElement) =
        match this with 
        | ComplexTypeSpec.Enum spec -> spec.Reader value |> Result.map (fun r -> r, Annotations.Empty)
        | ComplexTypeSpec.Object spec -> spec.Reader value
        | ComplexTypeSpec.Tuple spec -> spec.Reader value
        | ComplexTypeSpec.DU spec -> spec.Reader value

    member this.CollectComplexTypes path refs : ImmutableDictionary<ComplexTypeSpec, Set<string list>> =
        match this with 
        | ComplexTypeSpec.Enum _ -> ImmutableDictionary.Empty
        | ComplexTypeSpec.Object spec -> spec.CollectComplexTypes path refs
        | ComplexTypeSpec.Tuple spec -> spec.CollectComplexTypes path refs
        | ComplexTypeSpec.DU spec -> spec.CollectComplexTypes path refs

and [<RequireQualifiedAccess; CustomEquality; NoComparison>] Conversion =
    | Ref of Conversion option ref
    | False
    | True
    | Type of TypeSpec
    | ComplexType of ComplexTypeSpec

    override this.Equals (other : obj) : bool =
        match other with
        | :? Conversion as other ->
            match this, other with 
            | Ref a, Ref b -> Object.ReferenceEquals(a, b)
            | False, False -> true
            | True, True -> true
            | Type a, Type b -> a.Equals b
            | ComplexType a, ComplexType b -> a.Equals b
            | _, _ -> false
        | _ -> false

    override this.GetHashCode () : int =
        match this with
        | Conversion.Ref ref -> System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode ref
        | Conversion.False -> false.GetHashCode()
        | Conversion.True -> true.GetHashCode()
        | Conversion.Type spec -> spec.GetHashCode()
        | Conversion.ComplexType spec -> spec.GetHashCode()

    member this.IsFalseSchema = 
        match this with 
        | Conversion.Ref ref -> ref.Value.Value.IsFalseSchema
        | Conversion.False -> true
        | _ -> false
         
    member this.BuildTypeSpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) =
        match this with 
        | Conversion.Ref ref -> ref.Value.Value.BuildTypeSpec packageName names
        | Conversion.False
        | Conversion.True -> 
            let schema = JsonObject()
            schema.Add("$ref", JsonValue.Create("pulumi.json#/Any"))
            schema, None
        | Conversion.Type spec -> spec.BuildTypeSpec packageName names
        | Conversion.ComplexType spec ->
            match names.TryGetValue spec with 
            | false, _ -> failwithf "Could not find name for %O" spec
            | true, name ->
                let schema = JsonObject()
                schema.Add("$ref", JsonValue.Create("#/types/" + packageName + ":index:" + name))
                schema, None

    member this.BuildPropertySpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) =
        match this with 
        | Conversion.Ref ref -> ref.Value.Value.BuildPropertySpec packageName names
        | Conversion.False
        | Conversion.True -> 
            let schema = JsonObject()
            schema.Add("$ref", JsonValue.Create("pulumi.json#/Any"))
            schema
        | Conversion.Type spec -> spec.BuildPropertySpec packageName names
        | Conversion.ComplexType spec ->
            match names.TryGetValue spec with 
            | false, _ -> failwithf "Could not find name for %O" spec
            | true, name ->
                let schema = JsonObject()
                schema.Add("$ref", JsonValue.Create("#/types/" + packageName + ":index:" + name))
                schema

    member this.Writer (value : Pulumi.Provider.PropertyValue) : JsonNode option =
        match this with 
        | Conversion.Ref ref -> ref.Value.Value.Writer value
        | Conversion.False -> failwith "All values fail against the false schema"
        | Conversion.True -> writeAny PrimitiveValidation.None value
        | Conversion.Type spec -> spec.Writer value
        | Conversion.ComplexType spec -> spec.Writer value

    member this.Reader (value : JsonElement) : Result<Pulumi.Provider.PropertyValue * Annotations, string> =
        match this with 
        | Conversion.Ref ref -> ref.Value.Value.Reader value
        | Conversion.False -> errorf "All values fail against the false schema"
        | Conversion.True -> readAny PrimitiveValidation.None value
        | Conversion.Type spec -> spec.Reader value
        | Conversion.ComplexType spec -> spec.Reader value

    // Returns a map of complex types and the paths they we're discoved on
    member this.CollectComplexTypes path (refs : ImmutableHashSet<_>): ImmutableDictionary<ComplexTypeSpec, Set<string list>> =
        match this with 
        | Conversion.Ref box -> 
            match refs.Contains box with
            | true -> ImmutableDictionary.Empty
            | false -> box.Value.Value.CollectComplexTypes path (refs.Add box)
        | Conversion.False -> ImmutableDictionary.Empty
        | Conversion.True -> ImmutableDictionary.Empty
        | Conversion.Type spec -> spec.CollectComplexTypes path refs
        | Conversion.ComplexType spec -> 
            let types = spec.CollectComplexTypes path refs
            match types.TryGetValue spec with 
            | false, _ -> types.Add(spec, Set.singleton path)
            | true, paths -> types.Add(spec, Set.add path paths)

    static member UnionComplexTypes (a : ImmutableDictionary<ComplexTypeSpec, Set<string list>>)  (b : ImmutableDictionary<ComplexTypeSpec, Set<string list>>) =        
        b
        |> Seq.fold (fun (a : ImmutableDictionary<ComplexTypeSpec, Set<string list>>) b -> 
            match a.TryGetValue b.Key with 
            | false, _ -> a.Add(b.Key, b.Value)
            | true, paths -> a.SetItem(b.Key, Set.union paths b.Value)
        ) a

and ConversionContext = {
    Type : Json.Schema.SchemaValueType option
    // True if object types should default to additional properties
    AdditionalProperties : bool
    Refs : Map<string, Conversion option ref>
    
} with 
    static member Default = { 
        Type = None
        AdditionalProperties = true 
        Refs = Map.empty
    }

    member this.AddRef path = {
        this with Refs = Map.add path (ref None) this.Refs
    }

    member this.SetRef path value =
        match this.Refs.TryFind path with 
        | Some box -> 
            box.contents <- Some value
            Conversion.Ref box
        | None -> failwithf "Failed to set $ref %s" path

    member this.Clear = { this with Type = None; AdditionalProperties = true }

type RootInformation = {
    BaseUri : Uri
    Document : JsonElement
}

let getType (keywords : KeywordCollection) : Json.Schema.SchemaValueType option =
    keywords
    |> pickKeyword<Json.Schema.TypeKeyword>
    |> Option.map (fun typ -> typ.Type)
       
let getDescription keywords = 
    match pickKeyword<Json.Schema.DescriptionKeyword> keywords with 
    | Some kw -> Some kw.Value
    | None -> None

let getTitle keywords = 
    match pickKeyword<Json.Schema.TitleKeyword> keywords with 
    | Some kw -> Some kw.Value
    | None -> None

let convertNullSchema (jsonSchema : Json.Schema.JsonSchema) : NullConversion =
    { 
        Description = getDescription jsonSchema.Keywords
    }

let convertBoolSchema (jsonSchema : Json.Schema.JsonSchema) : PrimitiveConversion =
    {
        Type = PrimitiveType.Boolean
        Description = getDescription jsonSchema.Keywords
        Validation = PrimitiveValidation.None
        Const = Choice1Of4 ()
    }

let convertStringSchema path (jsonSchema : Json.Schema.JsonSchema) : Conversion =
    let enum = pickKeyword<Json.Schema.EnumKeyword> jsonSchema.Keywords
    match enum with 
    | None -> 
        {
            Type = PrimitiveType.String
            Description = getDescription jsonSchema.Keywords
            Validation = PrimitiveValidation.FromKeywords jsonSchema.Keywords
            Const = Choice1Of4 ()
        }
        |> TypeSpec.Primitive
        |> Conversion.Type
    | Some enum ->
        let enumValues =
            enum.Values
            |> Seq.toList

        {   
            Path = path
            Type = PrimitiveType.String
            Description = getDescription jsonSchema.Keywords
            Title = getTitle jsonSchema.Keywords
            Values = enumValues
        }
        |> ComplexTypeSpec.Enum
        |> Conversion.ComplexType

let convertNumberSchema path isInteger (jsonSchema : Json.Schema.JsonSchema) : Conversion =
    let enum = pickKeyword<Json.Schema.EnumKeyword> jsonSchema.Keywords
    match enum with 
    | None -> 
        {
            Type = if isInteger then PrimitiveType.Integer else PrimitiveType.Number
            Description = getDescription jsonSchema.Keywords
            Validation = PrimitiveValidation.FromKeywords jsonSchema.Keywords
            Const = Choice1Of4 ()
        }
        |> TypeSpec.Primitive
        |> Conversion.Type
    | Some enum ->
        let enumValues =
            enum.Values
            |> Seq.toList

        {   
            Path = path
            Type = if isInteger then PrimitiveType.Integer else PrimitiveType.Number
            Description = getDescription jsonSchema.Keywords
            Title = getTitle jsonSchema.Keywords
            Values = enumValues
        }
        |> ComplexTypeSpec.Enum
        |> Conversion.ComplexType

let convertConst (root : RootInformation) (schema : Json.Schema.JsonSchema) (constKeyword : Json.Schema.ConstKeyword) : Conversion =
    // if we've got a "const" we can fill in the type
    match constKeyword.Value with 
    | :? JsonValue as value ->
        let typ, constant = 
            match value.TryGetValue<bool>() |> optionOfTry with
            | Some b -> PrimitiveType.Boolean, Choice2Of4 b 
            | None ->
                match value.TryGetValue<double>() |> optionOfTry with
                | Some n -> PrimitiveType.Number, Choice3Of4 n
                | None ->                    
                    match value.TryGetValue<string>() |> optionOfTry with
                    | Some s -> PrimitiveType.String, Choice4Of4 s
                    | None -> failwith "constant value wasn't a bool, number or string"

        { 
            Description = getDescription schema.Keywords
            Type = typ
            Validation = PrimitiveValidation.FromKeywords schema.Keywords
            Const = constant
        }
        |> TypeSpec.Primitive
        |> Conversion.Type
    | constValue -> 
        {
            Description = Some (sprintf "unhandled const schema value: %O" constValue)
            PrimitiveValidation = PrimitiveValidation.FromKeywords schema.Keywords
        }
        |> TypeSpec.Any
        |> Conversion.Type

let readRef  (root : RootInformation) (ref : Json.Schema.RefKeyword) : Json.Schema.JsonSchema * Json.Pointer.JsonPointer =
    //let newUri = Uri("schema.json", ref.Reference)
// var newUri = new Uri(context.Scope.LocalScope, Reference);
// var navigation = (newUri.OriginalString, context.InstanceLocation);
// if (context.NavigatedReferences.Contains(navigation))
//    throw new JsonSchemaException($"Encountered circular reference at schema location `{newUri}` and instance location `{context.InstanceLocation}`");
//
// var newBaseUri = new Uri(newUri.GetLeftPart(UriPartial.Query));
//
// JsonSchema? targetSchema = null;
// var targetBase = context.Options.SchemaRegistry.Get(newBaseUri) ??
//	                throw new JsonSchemaException($"Cannot resolve base schema from `{newUri}`");
//

    if ref.Reference.IsAbsoluteUri then
        failwith "Absoloute $refs are not implemented"

    let newUri = Uri(root.BaseUri, ref.Reference)
    match Json.Pointer.JsonPointer.TryParse newUri.Fragment with 
    | true, pointerFragment -> 
        match pointerFragment.Evaluate(root.Document) |> Option.ofNullable with
        | None -> failwithf "failed to find $ref %O" ref.Reference
        | Some subelement ->
            Json.Schema.JsonSchema.FromText(subelement.GetRawText()), 
            pointerFragment
    | _ -> 
        failwithf "failed to parse ref %s" newUri.Fragment

// if (JsonPointer.TryParse(newUri.Fragment, out var pointerFragment))
//	{
//		if (targetBase == null)
//			throw new JsonSchemaException($"Cannot resolve base schema from `{newUri}`");
//		
//		targetSchema = targetBase.FindSubschema(pointerFragment!, context.Options);
//	}
//	else
//	{
//		var anchorFragment = newUri.Fragment.Substring(1);
//		if (!AnchorKeyword.AnchorPattern.IsMatch(anchorFragment))
//			throw new JsonSchemaException($"Unrecognized fragment type `{newUri}`");
//
//		if (targetBase.Anchors.TryGetValue(anchorFragment, out var anchorDefinition))
//			targetSchema = anchorDefinition.Schema;
//	}
//
//	if (targetSchema == null)
//		throw new JsonSchemaException($"Cannot resolve schema `{newUri}`");
//
    
let handleKeyword<'T when 'T : not struct> func keywordsMap =
    keywordsMap
    |> Map.filter (fun _ list -> 
        match list with 
        | item :: _ when item.GetType() = typeof<'T> -> 
            list 
            |> Seq.cast<'T>
            |> func
            false
        | _ -> true)

let rec convertRef (root : RootInformation) context (schema : Json.Schema.JsonSchema) (ref : Json.Schema.RefKeyword) : Conversion =
    let refKey = ref.Reference.ToString()
    match context.Refs.TryFind refKey with 
    | Some box -> Conversion.Ref box
    | None ->

    let context = context.AddRef refKey

    // If this is a simple $ref by itself just convert the inner type    
    let simpleRef = 
        schema.Keywords 
        |> Seq.forall (fun kw ->
            kw.Equals ref || not (isValidationKeyword kw)
        )
        
    let subschema, path = readRef root ref

    if simpleRef then 
        convertSubSchema root context path subschema
    else 
        // This isn't a simple ref, we need to do a schema merge
        // we build a new _schema_ that merges the subschemas in ways that are possible to express in the type system (falling back to just any if we can't merge them)
        // then we send that newly built schema to be converted
        
        // If the subschema is just bool then we can just return Any or None
        match subschema.BoolValue |> Option.ofNullable with
        | Some false -> Conversion.False
        | Some true -> 
            {
                Description = None
                PrimitiveValidation = PrimitiveValidation.None
            }
            |> TypeSpec.Any
            |> Conversion.Type
        | None ->

        let allKeywords = 
            [schema; subschema]
            |> List.fold (fun keywords schema ->
                schema.Keywords
                |> Seq.fold (fun keywords keyword ->
                    // Don't add the current ref keyword we're dealing with 
                    if keyword.Equals ref then keywords
                    else 
                        let key = keyword.GetType().FullName
                        keywords |> Map.change key (function 
                            | None -> Some [keyword] 
                            | Some others -> Some (keyword :: others))
                ) keywords
            ) Map.empty

        // First go through any keywords that are unique, these can just be added to the new schema
        let newSchema = Json.Schema.JsonSchemaBuilder()
        let allKeywords =
            allKeywords
            |> Map.filter (fun _ list -> 
                match list with 
                | [] -> false
                | [x] -> newSchema.Add(x); false
                | _ -> true)

        // At this point allKeywords is made of keywords that are in multiple schemas
        let allKeywords = 
            allKeywords

            // First handle the "type" keyword, we can just union this
            |> handleKeyword<Json.Schema.TypeKeyword> (fun kws ->
                let types =
                    kws
                    |> Seq.map (fun kw -> kw.Type)
                    |> Seq.fold (fun overall t -> overall ||| t) (enum<Json.Schema.SchemaValueType> 0)
                newSchema.Add(Json.Schema.TypeKeyword(types))
            )

            // Next handle title and description, we just take these from the main schema ignoring the merged in ref
            |> handleKeyword<Json.Schema.TitleKeyword> (fun _ ->
                schema.Keywords 
                |> pickKeyword<Json.Schema.TitleKeyword>
                |> Option.iter newSchema.Add
            )    
            |> handleKeyword<Json.Schema.DescriptionKeyword> (fun _ ->
                schema.Keywords 
                |> pickKeyword<Json.Schema.DescriptionKeyword>
                |> Option.iter newSchema.Add
            )
    
            // Next find the "properties" keyword
            |> handleKeyword<Json.Schema.PropertiesKeyword> (fun kws ->
                    let props = 
                        kws
                        |> Seq.map (fun kw -> kw.Properties)
                        |> Seq.fold (fun properties next ->
                            next 
                            |> Seq.fold (fun properties property -> 
                                match Map.tryFind property.Key properties with 
                                | None -> Map.add property.Key property.Value properties
                                | Some _ -> failwith "Property key conflict in allOf"
                            ) properties
                        ) Map.empty
                    newSchema.Add(Json.Schema.PropertiesKeyword(props))
            )

        if allKeywords.Count <> 0 then            
            {
                Description = Some (sprintf "Needs more translation %O" allKeywords)
                PrimitiveValidation = PrimitiveValidation.FromKeywords schema.Keywords
            }
            |> TypeSpec.Any
            |> Conversion.Type
        else 
            convertSubSchema root context path (newSchema.Build())
    |> fun result ->
        // Whatever the result of this ref set it
        context.SetRef refKey result

and convertArraySchema (root : RootInformation) (context : ConversionContext) (path : Json.Pointer.JsonPointer) (jsonSchema : Json.Schema.JsonSchema) : Conversion =    
    let prefixItems =
        jsonSchema.Keywords
        |> pickKeyword<Json.Schema.PrefixItemsKeyword>
        |> Option.map (fun kw -> 
            kw.ArraySchemas
            |> Seq.mapi (fun i s -> 
                let path = path.Combine(Pointer.attribute "prefixItems", Pointer.index i)
                convertSubSchema root context.Clear path s)
            |> Seq.toList
        )

    let items =
        jsonSchema.Keywords
        |> pickKeyword<Json.Schema.ItemsKeyword>
        |> Option.map (fun ik -> 
            let path = path.Combine(Pointer.attribute "items")
            convertSubSchema root context.Clear path ik.SingleSchema)

    let description = getDescription jsonSchema.Keywords
    match prefixItems with 
    | None ->
        // Basic array
        {
            Description = description
            Items = items
            Validation = ArrayValidation.FromKeywords jsonSchema.Keywords
        } |> TypeSpec.Array |> Conversion.Type
    | Some prefixItems -> 
        // Tuple type!
        {
            Path = path
            Title = getTitle jsonSchema.Keywords
            Description = description
            PrefixItems = prefixItems
            AdditionalItems = items
            Validation = ArrayValidation.FromKeywords jsonSchema.Keywords
        } |> ComplexTypeSpec.Tuple |> fun c -> Conversion.ComplexType c

and convertObjectSchema (root : RootInformation) (context : ConversionContext) (path : Json.Pointer.JsonPointer) (jsonSchema : Json.Schema.JsonSchema) : Conversion =
    let propertiesKeyword = jsonSchema.Keywords |> pickKeyword<Json.Schema.PropertiesKeyword>
    let additionalPropertiesKeyword = jsonSchema.Keywords |> pickKeyword<Json.Schema.AdditionalPropertiesKeyword>
    let requiredKeyword = jsonSchema.Keywords |> pickKeyword<Json.Schema.RequiredKeyword>
    let unevaluatedPropertiesKeyword = jsonSchema.Keywords |> pickKeyword<Json.Schema.UnevaluatedPropertiesKeyword>

    let properties = 
        propertiesKeyword
        |> Option.map (fun pk ->
            pk.Properties
            |> Seq.map (fun kv ->
                let path = path.Combine(Pointer.attribute "properties", Pointer.attribute kv.Key)
                kv.Key, convertSubSchema root context.Clear path kv.Value
            )
            |> Map.ofSeq
        )

    // Pulumi schema only directly supports maps (i.e additionalProperties is Some and properties = []) or fixed property bags (i.e. additionalProperties is None)
    // If we have both we need to add an extra "additionalProperties" property to the object
    let additionalProperties =
        match context.AdditionalProperties, additionalPropertiesKeyword with 
        | true, Some apk -> 
            let path = path.Combine(Pointer.attribute "additionalProperties")
            Some (convertSubSchema root context.Clear path apk.Schema)
        | true, None -> Some (Conversion.True)
        | false, _ -> None

    let required = 
        match requiredKeyword with
        | None -> Seq.empty
        | Some required -> required.Properties
        |> Set.ofSeq 
        
    let description = getDescription jsonSchema.Keywords
    let title = getTitle jsonSchema.Keywords

    match properties, additionalProperties with 
    | None, aps -> 
        // A map
        {
            Description = description
            AdditionalProperties = aps
        }
        |> TypeSpec.Map
        |> Conversion.Type
    | Some props, aps ->
        // We need to find unique Pulumified names for each property
        let props =
            Map.fold (fun (names, newProps) key value -> 
                let pulumiName = 
                    let name = cleanTextForName key
                    if Set.contains name names |> not then 
                        name
                    else 
                        let mutable index = 0
                        while Set.contains (sprintf "%s%d" name index) names do
                            index <- index + 1
                        sprintf "%s%d" name index                

                Set.add pulumiName names, Map.add pulumiName (key, value) newProps
            ) (Set.empty, Map.empty) props
            |> snd

        let aps = 
            match context.AdditionalProperties, aps with
            | true, aps -> Choice2Of2 aps
            | false, Some aps -> Choice2Of2 (Some aps)
            | false, None -> Choice1Of2 ()

        {
            Path = path
            Description = description
            Title = title
            Properties = props
            AdditionalProperties = aps
            Required = required
            Choices = []
        }
        |> ComplexTypeSpec.Object
        |> Conversion.ComplexType

and convertAllOf (root : RootInformation) context path (schema : Json.Schema.JsonSchema) (allOf : Json.Schema.AllOfKeyword): Conversion =

    // allOf is a union of all the subschemas
    // we build a new _schema_ that merges the subschemas in ways that are possible to express in the type system (falling back to just any if we can't merge them)
    // then we send that newly built schema to be converted
    let boolSchemas, keywordSchemas = schema :: Seq.toList allOf.Schemas |> List.partition (fun s -> s.BoolValue.HasValue)

    // If _any_ are false we can early out
    match boolSchemas |> List.map (fun s -> s.BoolValue.Value) |> List.exists not with 
    | true -> Conversion.False
    | false ->

    let allKeywords = 
        keywordSchemas
        |> List.fold (fun keywords schema ->
            schema.Keywords
            |> Seq.fold (fun keywords keyword ->
                // Don't add the current allOf keyword we're dealing with 
                if keyword.Equals(allOf) then keywords
                else 
                    let key = keyword.GetType().FullName
                    keywords |> Map.change key (function 
                        | None -> Some [keyword] 
                        | Some others -> Some (keyword :: others))
            ) keywords
        ) Map.empty

    // First go through any keywords that are unique, these can just be added to the new schema
    let newSchema = Json.Schema.JsonSchemaBuilder()
    let allKeywords =
        allKeywords
        |> Map.filter (fun _ list -> 
            match list with 
            | [] -> false
            | [x] -> 
                newSchema.Add(x)
                false
            | _ -> true)

    // At this point allKeywords is made of keywords that are in multiple schemas
    
    // First find the "properties" keyword
    let allKeywords = 
        allKeywords
        |> handleKeyword<Json.Schema.PropertiesKeyword>(fun kws ->
            let props = 
                kws
                |> Seq.map (fun kw -> kw.Properties)
                |> Seq.fold (fun properties next ->
                    next 
                    |> Seq.fold (fun properties property -> 
                        match Map.tryFind property.Key properties with 
                        | None -> Map.add property.Key property.Value properties
                        | Some _ -> failwith "Property key conflict in allOf"
                    ) properties
                ) Map.empty
            newSchema.Add(Json.Schema.PropertiesKeyword(props))
        )

    if allKeywords.Count <> 0 then
        failwithf "Needs more translation %O" allKeywords

    convertSubSchema root context path (newSchema.Build()) 

and convertOneOf (root : RootInformation) context (path : Json.Pointer.JsonPointer) (schema : Json.Schema.JsonSchema) (oneOf : Json.Schema.OneOfKeyword) : Conversion =

    // See if there's any validation keywords at this level or if it's just a simple oneOf
    let simpleOneOf = 
        schema.Keywords 
        |> Seq.forall (fun kw ->
            kw.Equals oneOf || not (isValidationKeyword kw)
        )

    let subConversions = 
        oneOf.Schemas 
        |> Seq.mapi (fun i subschema ->
            let path = path.Combine(Pointer.attribute "oneOf", Pointer.index i)            
            convertSubSchema root context path subschema)
        |> Seq.filter (fun c -> not c.IsFalseSchema)
        |> Seq.toList
        
    // Check to see if all the oneOf schemas are _simple_ and we don't have any extra validation if so we can just make this a type union
    let simpleUnion = 
        if not simpleOneOf then None 
        else
            subConversions 
            |> List.fold (fun (overall : UnionConversion option) subconversion -> 
                match overall with 
                | None -> None
                | Some overall ->
                    match subconversion with 
                    | Conversion.Type (TypeSpec.Primitive p) ->
                        let conversion = UnionConversion.OfPrimitive p
                        overall.TryMerge conversion
                    | Conversion.Type (TypeSpec.Union u) ->
                        overall.TryMerge u
                    | _ -> None
            ) (Some { Description = None; BooleanConversion = None; IntegerConversion = None; NumberConversion = None; StringConversion = None })

    match simpleOneOf, simpleUnion with
    | _, Some union -> union |> TypeSpec.Union |> Conversion.Type
    | true, _ ->
        // Emit a DU for each of the choices
        {
            Path = path
            Title = getTitle schema.Keywords
            Description = getDescription schema.Keywords
            Choices = subConversions
        }
        |> ComplexTypeSpec.DU
        |> Conversion.ComplexType
    | false, _ -> 
        // This is a complex object. First try to translate this object _without_ the oneOf
        let newSchema = Json.Schema.JsonSchemaBuilder()
        schema.Keywords 
        |> Seq.iter (fun keyword ->
            if keyword.Equals oneOf then ()
            else newSchema.Add keyword
        )
        let obj = convertSubSchema root context path (newSchema.Build()) 

        match obj with 
        | Conversion.ComplexType (ComplexTypeSpec.Object obj) ->
            // this _should_ be an object now, 
            let subConversions = 
                oneOf.Schemas 
                |> Seq.mapi (fun i subschema ->
                    let path = path.Combine(Pointer.attribute "oneOf", Pointer.index i)
                    convertSubSchema root { context with AdditionalProperties = false } path subschema)
                |> Seq.toList

            { obj with Choices = subConversions }
            |> ComplexTypeSpec.Object
            |> Conversion.ComplexType
        | root ->             
            {
                Description = Some (sprintf "unhandled root conversion for oneOf: %A" root)
                PrimitiveValidation = PrimitiveValidation.FromKeywords schema.Keywords
            }
            |> TypeSpec.Any
            |> Conversion.Type

and convertSubSchema (root : RootInformation) (context : ConversionContext) path (schema : Json.Schema.JsonSchema)  : Conversion =
    match schema.BoolValue |> Option.ofNullable with
    | Some false -> Conversion.False
    | Some true -> Conversion.True
    | None ->
        let keywords = schema.Keywords

        // If there are no validation keywords this is a simple any
        match keywords |> Seq.forall (isValidationKeyword >> not) with 
        | true ->                         
            {
                Description = getDescription keywords
                PrimitiveValidation = PrimitiveValidation.None
            }
            |> TypeSpec.Any
            |> Conversion.Type
        | false -> 

        let typ = 
            match getType keywords, context.Type with 
            | None, None -> None
            | Some t, None -> Some t
            | None, Some t -> Some t
            | Some a, Some b -> Some (a &&& b)

        if typ.IsSome && typ.Value = enum<Json.Schema.SchemaValueType> 0 then Conversion.False
        else

        let context = { context with Type = typ }

        let allOf = pickKeyword<Json.Schema.AllOfKeyword> keywords
        let oneOf = pickKeyword<Json.Schema.OneOfKeyword> keywords
        let ref = pickKeyword<Json.Schema.RefKeyword> keywords
        let constKeyword = pickKeyword<Json.Schema.ConstKeyword> keywords

        let hasType c =
            match typ with
            | Some t -> t.HasFlag c
            | None -> true
        
        if constKeyword.IsSome then 
            convertConst root schema constKeyword.Value
        elif ref.IsSome then
            convertRef root context schema ref.Value
        elif allOf.IsSome then
            convertAllOf root context path schema allOf.Value
        elif oneOf.IsSome then
            convertOneOf root context path schema oneOf.Value
        else 
            let nullSchema = 
                if hasType Json.Schema.SchemaValueType.Null then
                    convertNullSchema schema |> TypeSpec.Null |> Conversion.Type |> Some
                else 
                    None

            let boolSchema = 
                if hasType Json.Schema.SchemaValueType.Boolean then 
                    convertBoolSchema schema |> TypeSpec.Primitive |> Conversion.Type |> Some
                else
                    None

            let stringSchema = 
                if hasType Json.Schema.SchemaValueType.String then 
                    convertStringSchema path schema |> Some
                else 
                    None

            let intSchema = 
                if hasType Json.Schema.SchemaValueType.Integer then 
                    convertNumberSchema path true schema |> Some
                else 
                    None

            let numSchema = 
                if hasType Json.Schema.SchemaValueType.Number then 
                    convertNumberSchema path false schema |> Some
                else 
                    None

            let objSchema = 
                if hasType Json.Schema.SchemaValueType.Object then 
                    convertObjectSchema root context path schema |> Some
                else 
                    None

            let arrSchema = 
                if hasType Json.Schema.SchemaValueType.Array then 
                    convertArraySchema root context path schema |> Some
                else
                    None

            let (|MaybePrimitive|) (value : Conversion option) =
                match value with
                | Some (Conversion.Type (TypeSpec.Primitive x)) -> Some x
                | _ -> None

            match nullSchema, boolSchema, stringSchema, intSchema, numSchema, objSchema, arrSchema with
            // No type valid, just return false
            | None, None, None, None, None, None, None -> Conversion.False
            // Just one type valid, return it
            | Some c, None, None, None, None, None, None -> c
            | None, Some c, None, None, None, None, None -> c
            | None, None, Some c, None, None, None, None -> c
            | None, None, None, Some c, None, None, None -> c
            | None, None, None, None, Some c, None, None -> c
            | None, None, None, None, None, Some c, None -> c
            | None, None, None, None, None, None, Some c -> c
            // Only basic types
            | None, MaybePrimitive b, MaybePrimitive s, MaybePrimitive i, MaybePrimitive n, None, None ->
                {
                    Description = None
                    BooleanConversion = b
                    StringConversion = s
                    IntegerConversion = i
                    NumberConversion = n
                }
                |> TypeSpec.Union
                |> Conversion.Type
            | _ -> 
                let msg =
                    keywords
                    |> Seq.map (fun kw -> kw.ToString())
                    |> String.concat ", "
                    |> sprintf "unhandled schema: %s"

                {
                    Description = Some msg
                    PrimitiveValidation = PrimitiveValidation.FromKeywords schema.Keywords
                }
                |> TypeSpec.Any 
                |> Conversion.Type

type RootConversion = {
    Schema: JsonObject
    Writer : Pulumi.Provider.PropertyValue -> JsonNode option
    Reader : JsonElement ->  Pulumi.Provider.PropertyValue
}

// Generate a full pulumi schema using the conversion as the function to generate
let convertSchema (uri : Uri) (jsonSchema : JsonElement) : RootConversion =
    let schema = JsonObject()
    let packageName = uri.Segments |> Seq.last |> System.IO.Path.GetFileNameWithoutExtension
    schema.Add("name", JsonValue.Create(packageName))
    schema.Add("description", JsonValue.Create("A pulumi package generated from a json schema"))
    schema.Add("keywords", JsonArray(
        JsonValue.Create("pulumi"),
        JsonValue.Create("jsonschema")))
    schema.Add("homepage", JsonValue.Create("https://github.com/Frassle/pulumi-jsonschema"))
    schema.Add("repository", JsonValue.Create("https://github.com/Frassle/pulumi-jsonschema"))
    schema.Add("license", JsonValue.Create("Apache-2.0"))
    let functions = JsonObject()
    schema.Add("functions", functions)
    let readFunction = JsonObject()
    let writeFunction = JsonObject()
    functions.Add(packageName + ":index:read", readFunction)
    functions.Add(packageName + ":index:write", writeFunction)

    let root = {
        BaseUri = uri
        Document = jsonSchema
    }
    let jsonSchema = Json.Schema.JsonSchema.FromText (jsonSchema.GetRawText())
    let conversion = convertSubSchema root ConversionContext.Default Json.Pointer.JsonPointer.Empty jsonSchema 

    // We need to get all complex types and make names for them, then ask for the root schema to generate a pulumi schema for itself _given_ those names
    let complexTypes = conversion.CollectComplexTypes [] ImmutableHashSet.Empty
    let usedNames = System.Collections.Generic.HashSet()
    let names = System.Collections.Generic.Dictionary()
    for kv in complexTypes do
        let complexType, paths = kv.Key, kv.Value

        let nameRegex = System.Text.RegularExpressions.Regex("^[a-z][A-Za-z0-9]+$")

        let isValid name =
            if usedNames.Contains name then None 
            elif not (nameRegex.IsMatch name) then None
            else Some name

        let name = 
            // If we have a title use that
            complexType.Title
            |> Option.map cleanTextForName
            |> Option.bind isValid
            
            // Else use the schema path of the type
            |> Option.orElseWith (fun () ->
                complexType.Path.Segments
                |> Seq.toList
                |> List.map (fun seg -> seg.Source)
                |> List.filter (fun seg -> seg <> "$defs")
                |> List.rev
                |> allPrefixes
                |> Seq.map (fun prefix ->
                    prefix
                    |> List.rev
                    |> String.concat "_"
                    |> cleanTextForName
                )
                |> Seq.tryPick isValid
            )

            // Else try the attribute paths 
            |> Option.orElseWith (fun () ->
                paths
                |> Seq.tryPick (fun path ->
                    path
                    |> allPrefixes
                    |> Seq.map (fun prefix ->
                        prefix
                        |> List.rev
                        |> String.concat "_"
                        |> cleanTextForName
                    )
                    |> Seq.tryPick isValid
                )
            )

            // Default to "root"
            |> Option.defaultValue "root"

        let freename = 
            if usedNames.Contains name |> not then name
            else 
                let mutable suffix = 0
                while usedNames.Contains (sprintf "%s%d" name suffix) do
                    suffix <- suffix + 1
                sprintf "%s%d" name suffix

        usedNames.Add(freename) |> ignore
        names.Add(complexType, freename)
            
    let names = ImmutableDictionary.CreateRange(names)    
    if not names.IsEmpty then
        let types = JsonObject()
        schema.Add("types", types)
        for kv in names do
            let complexTypeSpec = kv.Key.BuildComplexTypeSpec packageName names
            types.Add(packageName + ":index:" + kv.Value, complexTypeSpec)
    
    // if the schema is a valid complex type then embed it into types and return that, else we'll embed it directly
    let names = ImmutableDictionary.CreateRange(names)
    let readObjectType = conversion.BuildPropertySpec packageName names
    let writeObjectType = conversion.BuildPropertySpec packageName names

    let jsonProperty() =
        let node = JsonObject()
        node.Add("type", JsonValue.Create("string"))
        node

    let mkkv name node = KeyValuePair.Create<string, JsonNode>(name, node)

    readFunction.Add("description", "Read the given JSON into the object model")
    readFunction.Add("inputs", JsonObject([
        mkkv "required" (JsonArray(JsonValue.Create "json"))
        mkkv "properties" (JsonObject([
             mkkv "json" (jsonProperty())
        ]))
    ]))
    readFunction.Add("outputs", JsonObject([
        mkkv "required" (JsonArray(JsonValue.Create "value"))
        mkkv "properties" (JsonObject([
             mkkv "value" readObjectType
        ]))
    ]))

    writeFunction.Add("description", "Read the given JSON into the object model")
    writeFunction.Add("inputs", JsonObject([
        mkkv "required" (JsonArray(JsonValue.Create "value"))
        mkkv "properties" (JsonObject([
             mkkv "value" writeObjectType
        ]))
    ]))
    writeFunction.Add("outputs", JsonObject([
        mkkv "required" (JsonArray(JsonValue.Create "json"))
        mkkv "properties" (JsonObject([
             mkkv "json" (jsonProperty())
        ]))
    ]))

    let reader (element : JsonElement) =
        match conversion.Reader element with 
        | Ok (ok, _) -> ok
        | Error msg -> failwith msg

    {
        Schema = schema
        Reader = reader
        Writer = conversion.Writer
    }
