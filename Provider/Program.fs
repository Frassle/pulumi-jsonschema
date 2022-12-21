// Given a JSON schema generate a Pulumi schema, and a way to read/write from that pulumi type to the expected json.


module Provider

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
    let text = text.Replace(" ", "_").Replace("-", "_").Trim('_')
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
            | Some l when uint value.Length >= l -> 
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
            | Some m when Decimal.Remainder(value, m) <> 0 -> 
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
} with 
    static member None = { String = StringValidation.None; Numeric = NumericValidation.None }

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
    getNode value

let readPrimitive (typ: PrimitiveType) (validation : PrimitiveValidation)  (value : JsonElement) =
    match typ with 
    | PrimitiveType.Boolean ->
        if value.ValueKind = JsonValueKind.True then
            Ok (Pulumi.Provider.PropertyValue true)
        elif value.ValueKind = JsonValueKind.False then
            Ok (Pulumi.Provider.PropertyValue false)
        else 
            errorf "Invalid JSON document expected bool got %O" value.ValueKind
    | PrimitiveType.Integer
    | PrimitiveType.Number ->
        if value.ValueKind = JsonValueKind.Number then
            let num = value.GetDecimal()
            match validation.Numeric.Validate num with
            | Some err -> Error err
            | None -> Ok (float num)
            |> Result.map Pulumi.Provider.PropertyValue
        else 
            errorf "Invalid JSON document expected number got %O" value.ValueKind
    | PrimitiveType.String ->
        if value.ValueKind = JsonValueKind.String then
            let str = value.GetString()
            match validation.String.Validate str with
            | Some err -> Error err
            | None -> Ok str
            |> Result.map Pulumi.Provider.PropertyValue
        else 
            errorf "Invalid JSON document expected number got %O" value.ValueKind

type AnyConversion = {
    Description : string option
    Type : PrimitiveType    
}

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
    NumberConversion : PrimitiveConversion option
    StringConversion : PrimitiveConversion option
} with
    static member OfPrimitive (conversion : PrimitiveConversion) =
        match conversion.Type with
        | PrimitiveType.Boolean -> { Description = None; BooleanConversion = Some conversion; NumberConversion = None; StringConversion = None }
        | PrimitiveType.Integer
        | PrimitiveType.Number -> { Description = None; BooleanConversion = None; NumberConversion = Some conversion; StringConversion = None }
        | PrimitiveType.String -> { Description = None; BooleanConversion = None; NumberConversion = None; StringConversion = Some conversion }

    member this.TryMerge (other : UnionConversion) =
        let unique a b = 
            match a, b with 
            | None, None -> Result.Ok None
            | Some a, None -> Result.Ok (Some a)
            | None, Some b -> Result.Ok (Some b)
            | _, _ -> Result.Error ()
        
        let boolean = unique this.BooleanConversion other.BooleanConversion 
        let number = unique this.NumberConversion other.NumberConversion 
        let str = unique this.StringConversion other.StringConversion

        match boolean, number, str with 
        | Ok b, Ok n, Ok s -> 
            Some { Description = this.Description; BooleanConversion = b; NumberConversion = n; StringConversion = s }
        | _ -> None

    member this.BuildTypeSpec () =
        let schema = JsonObject()

        let oneof = JsonArray()
        [this.BooleanConversion; this.NumberConversion; this.StringConversion]
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
        [this.BooleanConversion; this.NumberConversion; this.StringConversion]
        |> List.iter (function 
            | None -> ()
            | Some conversion -> 
                let spec, _ = conversion.BuildTypeSpec()
                oneof.Add(spec))
        schema.Add("oneOf", oneof)
                
        this.Description |> Option.iter (fun desc -> schema.Add("description", desc))
        schema

    member private this.expectedTypes = 
        ["number", this.NumberConversion; "string", this.StringConversion; "boolean", this.BooleanConversion]
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
            (fun _ -> 
                match this.NumberConversion with 
                | None -> raise ()
                | Some conversion -> conversion.Writer value
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
        elif value.ValueKind = JsonValueKind.Number && this.NumberConversion.IsSome then
            this.NumberConversion.Value.Reader value
        elif value.ValueKind = JsonValueKind.String && this.StringConversion.IsSome then
            this.StringConversion.Value.Reader value
        else 
            failwithf "Invalid JSON document expected %s got %O" this.expectedTypes value.ValueKind

type EnumConversion = {
    Path : string list
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
        writePrimitive this.Type PrimitiveValidation.None value

    member this.Reader (value : JsonElement) = 
        readPrimitive this.Type PrimitiveValidation.None value

type ConversionContext = {
    Type : Json.Schema.SchemaValueType option
    // True if object types should default to additional properties
    AdditionalProperties : bool
} with 
    static member Default = { Type = None; AdditionalProperties = true }

                
type ArrayConversion = {
    Description : string option
    Items : Conversion
} with
    member this.BuildTypeSpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) =
        let schema = JsonObject()
        schema.Add("type", JsonValue.Create("array"))
        let items, desc = this.Items.BuildTypeSpec packageName names
        schema.Add("items", items)
        schema, this.Description

    member this.BuildPropertySpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) = 
        let schema = JsonObject()
        schema.Add("type", JsonValue.Create("array"))
        let items, desc = this.Items.BuildTypeSpec packageName names
        schema.Add("items", items)
        this.Description |> Option.iter (fun desc -> schema.Add("description", desc))
        schema
        
    member this.Writer (value : Pulumi.Provider.PropertyValue) = 
        let maybeArr = 
            value.TryUnwrap ()
            |> optionOfTry
            |> Option.bind (fun v -> v.TryGetArray() |> optionOfTry)

        match maybeArr with 
        | None -> failwithf "Invalid type expected array got %O" value.Type
        | Some arr ->
            arr
            |> Seq.map (fun item ->
                this.Items.Writer item
                |> Option.toObj
            )
            |> Seq.toArray
            |> JsonArray
            :> JsonNode
            |> Some

    member this.Reader (value : JsonElement) =
        if value.ValueKind = JsonValueKind.Array then
            value.EnumerateArray()
            |> Seq.map (fun item -> this.Items.Reader item)
            |> Seq.toList
            |> okList
            |> Result.mapError (fun errs -> String.concat ", " errs)
            |> Result.map (fun items ->
                items
                |> ImmutableArray.CreateRange
                |> Pulumi.Provider.PropertyValue)
        else 
            errorf "Invalid JSON document expected array got %O" value.ValueKind

    member this.CollectComplexTypes() : ImmutableHashSet<ComplexTypeSpec> =
        this.Items.CollectComplexTypes()

and MapConversion = {
    Description : string option
    AdditionalProperties : Conversion
} with
    member this.BuildTypeSpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) = 
        let schema = JsonObject()
        schema.Add("type", JsonValue.Create("object"))
        let additionalProperties, desc = this.AdditionalProperties.BuildTypeSpec packageName names
        schema.Add("additionalProperties", additionalProperties)
        schema, this.Description

    member this.BuildPropertySpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) = 
        let schema = JsonObject()
        schema.Add("type", JsonValue.Create("object"))
        let additionalProperties, desc = this.AdditionalProperties.BuildTypeSpec packageName names
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
            obj
            |> Seq.map (fun kv ->
                let node = this.AdditionalProperties.Writer kv.Value
                match node with
                | Some node -> KeyValuePair.Create(kv.Key, node)
                | None -> KeyValuePair.Create(kv.Key, null)
            )
            |> JsonObject
            :> JsonNode
            |> Some

    member this.Reader (value : JsonElement) = 
        if value.ValueKind = JsonValueKind.Object then
            value.EnumerateObject()
            |> Seq.map (fun kv -> 
                match this.AdditionalProperties.Reader kv.Value with 
                | Ok ok -> Ok (KeyValuePair.Create(kv.Name, ok))
                | Error err -> Error err)
            |> Seq.toList
            |> okList
            |> Result.mapError (fun errs -> String.concat ", " errs)
            |> Result.map (fun items -> 
                items
                |> ImmutableDictionary.CreateRange
                |> Pulumi.Provider.PropertyValue)
        else 
            errorf "Invalid JSON document expected object got %O" value.ValueKind

    member this.CollectComplexTypes() : ImmutableHashSet<ComplexTypeSpec> =
        this.AdditionalProperties.CollectComplexTypes()

and [<RequireQualifiedAccess>] TypeSpec = 
    | Any of string option
    | Null of NullConversion
    | Primitive of PrimitiveConversion
    | Array of ArrayConversion
    | Map of MapConversion
    | Union of UnionConversion

    member this.BuildTypeSpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) = 
        match this with 
        | Any desc ->
            let schema = JsonObject()
            schema.Add("$ref", JsonValue.Create("pulumi.json#/Any"))
            schema, desc
        | Null c -> c.BuildTypeSpec()
        | Primitive c -> c.BuildTypeSpec()
        | Array c -> c.BuildTypeSpec packageName names
        | Map c -> c.BuildTypeSpec packageName names
        | Union c -> c.BuildTypeSpec()

    member this.BuildPropertySpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) = 
        match this with 
        | Any desc ->
            let schema = JsonObject()
            schema.Add("$ref", JsonValue.Create("pulumi.json#/Any"))
            desc |> Option.iter (fun desc -> schema.Add("description", desc))
            schema
        | Null c -> c.BuildPropertySpec()
        | Primitive c -> c.BuildPropertySpec()
        | Array c -> c.BuildPropertySpec packageName names
        | Map c -> c.BuildPropertySpec packageName names
        | Union c -> c.BuildPropertySpec()
        
    member this.Writer (value : Pulumi.Provider.PropertyValue) = 
        match this with 
        | Any _ -> 
            let rec getValue (value : Pulumi.Provider.PropertyValue) =
                value.Match<JsonNode option>(
                    (fun () -> None),
                    (fun b -> JsonValue.Create(b) :> JsonNode |> Some),
                    (fun n -> JsonValue.Create(n) :> JsonNode |> Some),
                    (fun s -> JsonValue.Create(s) :> JsonNode |> Some),
                    (fun a -> 
                        a
                        |> Seq.map (fun i -> getValue i |> Option.toObj)
                        |> Seq.toArray
                        |> JsonArray
                        :> JsonNode
                        |> Some
                    ),
                    (fun o -> 
                        o
                        |> Seq.map (fun kv -> 
                            KeyValuePair.Create(kv.Key, getValue kv.Value |> Option.toObj)
                        )
                        |> Seq.toArray
                        |> JsonObject
                        :> JsonNode
                        |> Some
                    ),
                    (fun _ -> failwith "not implemented"),
                    (fun _ -> failwith "not implemented"),
                    (fun secret -> getValue secret),
                    (fun _ -> failwith "not implemented"),   
                    (fun output -> getValue output.Value),
                    (fun _ ->failwith "not implemented")
                )
            getValue value
        | Null c -> c.Writer value
        | Primitive c -> c.Writer value
        | Array c -> c.Writer value
        | Map c -> c.Writer value        
        | Union c -> c.Writer value
        
    member this.Reader (value : JsonElement) = 
        match this with 
        | Any _ -> 
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
                |> Seq.map (fun item -> this.Reader item)
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
                    match this.Reader item.Value with 
                    | Ok v -> Ok (KeyValuePair.Create(item.Name, v))
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
        | Null c -> c.Reader value
        | Primitive c -> c.Reader value
        | Array c -> c.Reader value
        | Map c -> c.Reader value 
        | Union c -> c.Reader value

    member this.CollectComplexTypes() : ImmutableHashSet<ComplexTypeSpec> =
        match this with 
        | Any _ -> ImmutableHashSet.Empty
        | Null c -> ImmutableHashSet.Empty
        | Primitive c -> ImmutableHashSet.Empty
        | Array c -> c.CollectComplexTypes()
        | Map c -> c.CollectComplexTypes()
        | Union c -> ImmutableHashSet.Empty

and DiscriminateUnionConversion = {
    Path : string list
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
            | Ok v -> Some (KeyValuePair.Create(sprintf "choice%dOf%d" (i+1) this.Choices.Length, v))
            | Error e -> None
        )
        |> function 
           | [result] -> Ok (Pulumi.Provider.PropertyValue(ImmutableDictionary.CreateRange [result]))
           | results -> errorf "Expected 1 matching subschema but found %d" results.Length

    member this.CollectComplexTypes() : ImmutableHashSet<ComplexTypeSpec> =
        this.Choices
        |> Seq.fold (fun types choice -> choice.CollectComplexTypes().Union(types)
        ) ImmutableHashSet.Empty

and ObjectConversion = {
    Path : string list
    Title : string option
    Description : string option
    Properties : Map<string, string * Conversion>
    AdditionalProperties : Conversion option
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
            propertiesSchema.Add(kv.Key, prop.BuildPropertySpec packageName names)
            
        match this.AdditionalProperties with 
        | None -> ()
        | Some aps ->
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

            obj
            |> Seq.collect (fun kv ->
                match this.AdditionalProperties with
                | None -> 
                    match this.Properties.TryFind kv.Key with 
                    | Some (jsonName, conversion) ->
                        let node = conversion.Writer kv.Value
                        match node with 
                        | Some node -> [KeyValuePair.Create(jsonName, node)] :> seq<_>
                        | None -> [KeyValuePair.Create(jsonName, null)]
                    | None -> failwithf "unexpected property '%s'" kv.Key
                | Some additionalProperties ->
                    if kv.Key = "additionalProperties" then
                        // This _should_ be an object
                        match kv.Value.TryGetObject() with
                        | false, _ -> failwithf "Invalid type expected object got %O" kv.Value
                        | true, additionalObj ->
                            additionalObj
                            |> Seq.map (fun kv ->
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

            let choice = 
                match this.Choices with
                | [] -> None 
                | choices -> 
                    choices
                    |> List.mapi (fun i choice -> i, choice)
                    |> List.choose (fun (i, choice) ->
                        match choice.Reader value with 
                        | Ok v -> Some (KeyValuePair.Create(sprintf "choice%dOf%d" (i+1) this.Choices.Length, v))
                        | Error e -> None
                    )
                    |> function 
                       | [result] -> Ok result
                       | results -> errorf "Expected 1 matching subschema but found %d" results.Length
                    |> Some

            let propertiesAdditionalProperties =
                value.EnumerateObject()
                |> Seq.fold (fun props kv ->
                    match props with 
                    | Error err -> Error err
                    | Ok (propsSoFar, addPropsSoFar) ->
                        match this.AdditionalProperties with
                        | None -> 
                            match tryFindName kv.Name with 
                            | Some (pulumiName, conversion) ->
                                match conversion.Reader kv.Value with
                                | Ok ok -> Ok (KeyValuePair.Create(pulumiName, ok) :: propsSoFar, [])
                                | Error err -> Error err
                            | None -> errorf "unexpected property '%s'" kv.Name
                        | Some additionalProperties ->
                            match tryFindName kv.Name with
                            | Some (pulumiName, conversion) ->
                                match conversion.Reader kv.Value with 
                                | Ok ok -> Ok (KeyValuePair.Create(pulumiName, ok) :: propsSoFar, addPropsSoFar)
                                | Error err -> Error err
                            | None ->                        
                                match additionalProperties.Reader kv.Value with  
                                | Ok ok -> Ok (propsSoFar, KeyValuePair.Create(kv.Name, ok) :: addPropsSoFar)
                                | Error err -> Error err
                ) (Ok ([], []))

            match propertiesAdditionalProperties with 
            | Error err -> Error err
            | Ok (properties, additionalProperties) ->

            // If we have any additionalProperties add them to the properties
            let properties = 
                match additionalProperties with
                | [] -> properties
                | additionalProperties ->
                    let arr = Pulumi.Provider.PropertyValue(ImmutableDictionary.CreateRange additionalProperties)
                    KeyValuePair.Create("additionalProperties", arr) :: properties
            
            // Add the choice to properties
            let properties = 
                match choice with
                | None -> Ok properties
                | Some (Ok choice) -> Ok (choice :: properties)
                | Some (Error err) -> Error err

            properties
            |> Result.map (
                ImmutableDictionary.CreateRange
                >> Pulumi.Provider.PropertyValue)
        else 
            failwithf "Invalid JSON document expected object got %O" value.ValueKind

    member this.CollectComplexTypes() : ImmutableHashSet<ComplexTypeSpec> =
        let complexTypes = 
            this.Properties
            |> Seq.fold (fun types kv -> 
                let (_, prop) = kv.Value
                prop.CollectComplexTypes().Union(types)
            ) ImmutableHashSet.Empty
        let additionalTypes =
            match this.AdditionalProperties with
            | None -> complexTypes
            | Some aps -> aps.CollectComplexTypes().Union(complexTypes)
            
        this.Choices
        |> Seq.fold (fun types kv ->
            kv.CollectComplexTypes().Union(types)
        ) additionalTypes

        
and TupleConversion = {
    Path : string list
    Title : string option
    Description : string option
    PrefixItems : Conversion list
    AdditionalItems : Conversion option
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

        match this.AdditionalItems with
        | None -> ()
        | Some ais ->
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
            let propertiesRest =
                value.EnumerateArray()
                |> Seq.mapi (fun i item -> (i, item))
                |> Seq.fold (fun propertiesRest (i, item) ->
                    match propertiesRest with 
                    | Error err -> Error err
                    | Ok (properties, rest) ->
                        match List.tryItem i this.PrefixItems, this.AdditionalItems with 
                        | None, None -> errorf "unexpected item %d in tuple" i
                        | None, Some ais -> 
                            match ais.Reader item with 
                            | Ok ok -> Ok (properties, ok :: rest)
                            | Error err -> Error err
                        | Some property, _ -> 
                            match property.Reader item with 
                            | Ok ok -> Ok (Map.add (i+1) ok properties, rest)
                            | Error err -> Error err
                ) (Ok (Map.empty, []))

            match propertiesRest with
            | Error err -> Error err
            | Ok (properties, rest) ->
                
            // If we have any additionalItems we'll add them to the properties
            let rest = 
                match this.AdditionalItems, rest with 
                | Some _, rest -> 
                    let arr = rest |> List.rev |> ImmutableArray.CreateRange
                    Ok (Some (KeyValuePair.Create("rest", Pulumi.Provider.PropertyValue(arr))))
                | None, [] -> Ok None
                | None, rest -> errorf "AdditionalItems isn't set, but we got values in rest"

            match rest with
            | Error err -> Error err
            | Ok rest ->

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

    member this.CollectComplexTypes() : ImmutableHashSet<ComplexTypeSpec> =
        let complexTypes = 
            this.PrefixItems
            |> Seq.fold (fun types item -> item.CollectComplexTypes().Union(types)
            ) ImmutableHashSet.Empty
        match this.AdditionalItems with
        | None -> complexTypes
        | Some ais -> ais.CollectComplexTypes().Union(complexTypes)

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
        | ComplexTypeSpec.Enum spec -> spec.Reader value
        | ComplexTypeSpec.Object spec -> spec.Reader value
        | ComplexTypeSpec.Tuple spec -> spec.Reader value
        | ComplexTypeSpec.DU spec -> spec.Reader value

    member this.CollectComplexTypes() : ImmutableHashSet<ComplexTypeSpec> =
        match this with 
        | ComplexTypeSpec.Enum _ -> ImmutableHashSet.Create(this)
        | ComplexTypeSpec.Object spec -> spec.CollectComplexTypes().Add(this)
        | ComplexTypeSpec.Tuple spec -> spec.CollectComplexTypes().Add(this)
        | ComplexTypeSpec.DU spec -> spec.CollectComplexTypes().Add(this)

and [<RequireQualifiedAccess>] Conversion =
    | Type of TypeSpec
    | ComplexType of ComplexTypeSpec
         
    member this.BuildTypeSpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) =
        match this with 
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
        | Conversion.Type spec -> spec.Writer value
        | Conversion.ComplexType spec -> spec.Writer value

    member this.Reader (value : JsonElement) : Result<Pulumi.Provider.PropertyValue, string> =
        match this with 
        | Conversion.Type spec -> spec.Reader value
        | Conversion.ComplexType spec -> spec.Reader value

    member this.CollectComplexTypes() : ImmutableHashSet<ComplexTypeSpec> =
        match this with 
        | Conversion.Type spec -> spec.CollectComplexTypes()
        | Conversion.ComplexType spec -> spec.CollectComplexTypes()

type RootInformation = {
    BaseUri : Uri
    Document : JsonElement
}

let getType (keywords : KeywordCollection) : Json.Schema.SchemaValueType option =
    keywords
    |> pickKeyword<Json.Schema.TypeKeyword>
    |> Option.map (fun typ -> typ.Type)

let isSimpleType (schemaValueType : Json.Schema.SchemaValueType) : bool =
    schemaValueType = Json.Schema.SchemaValueType.Null ||
    schemaValueType = Json.Schema.SchemaValueType.Boolean ||
    schemaValueType = Json.Schema.SchemaValueType.Integer ||
    schemaValueType = Json.Schema.SchemaValueType.Number ||
    schemaValueType = Json.Schema.SchemaValueType.String ||
    schemaValueType = Json.Schema.SchemaValueType.Array ||
    schemaValueType = Json.Schema.SchemaValueType.Object

let isSimpleUnion (keywords : KeywordCollection) : bool =
    keywords
    |> pickKeyword<Json.Schema.TypeKeyword>
    |> Option.map (fun typ ->
        not (typ.Type.HasFlag Json.Schema.SchemaValueType.Object) &&
        not (typ.Type.HasFlag Json.Schema.SchemaValueType.Array)
    ) |> Option.defaultValue false
        
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
            Validation = {
                String = StringValidation.FromKeywords jsonSchema.Keywords
                Numeric = NumericValidation.None
            }
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

let convertNumberSchema isInteger (jsonSchema : Json.Schema.JsonSchema) : PrimitiveConversion =
    {
        Type = if isInteger then PrimitiveType.Integer else PrimitiveType.Number
        Description = getDescription jsonSchema.Keywords
        Validation = {
            String = StringValidation.None
            Numeric = NumericValidation.FromKeywords jsonSchema.Keywords
        }
        Const = Choice1Of4 ()
    }

let convertSimpleUnion (jsonSchema : Json.Schema.JsonSchema) : UnionConversion =
    let typ = jsonSchema.Keywords |> pickKeyword<Json.Schema.TypeKeyword> |> Option.get
    
    let numberConversion =
        if typ.Type.HasFlag Json.Schema.SchemaValueType.Number then
            Some (convertNumberSchema false jsonSchema)
        else None

    let stringConversion =
        if typ.Type.HasFlag Json.Schema.SchemaValueType.String then
            match convertStringSchema [] jsonSchema with 
            | Conversion.Type spec -> 
                match spec with 
                | TypeSpec.Primitive p -> Some p
                | _ -> failwith "Expected convertStringSchema to return a PrimitiveConversion"
            | _ -> failwith "Expected convertStringSchema to return a PrimitiveConversion"
        else None

    let boolConversion =
        if typ.Type.HasFlag Json.Schema.SchemaValueType.Boolean then
            Some (convertBoolSchema jsonSchema)
        else None

    {
        Description = getDescription jsonSchema.Keywords
        NumberConversion = numberConversion
        StringConversion = stringConversion
        BooleanConversion = boolConversion
    }

let convertConst (root : RootInformation) path (schema : Json.Schema.JsonSchema) (constKeyword : Json.Schema.ConstKeyword) : Conversion =
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
            Validation = PrimitiveValidation.None
            Const = constant
        }
        |> TypeSpec.Primitive
        |> Conversion.Type
    | constValue -> 
        sprintf "unhandled const schema value: %O" constValue
        |>  Some
        |> TypeSpec.Any
        |> Conversion.Type

let readRef  (root : RootInformation) (ref : Json.Schema.RefKeyword) : Json.Schema.JsonSchema * string list =
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
            pointerFragment.Segments |> Array.map (fun seg -> seg.Value) |> Array.toList
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

let rec convertRef (root : RootInformation) path context (schema : Json.Schema.JsonSchema) (ref : Json.Schema.RefKeyword) : Conversion option =
    // If this is a simple $ref by itself just convert the inner type    
    let simpleRef = 
        schema.Keywords 
        |> Seq.forall (fun kw ->
            kw.Equals ref || not (isValidationKeyword kw)
        )
        
    let subschema, segments = readRef root ref

    if simpleRef then 
        // We need a new path here because this _ref_ could be seen by multiple paths
        convertSubSchema root (List.rev segments) context subschema
    else 
        // This isn't a simple ref, we need to do a schema merge
        // we build a new _schema_ that merges the subschemas in ways that are possible to express in the type system (falling back to just any if we can't merge them)
        // then we send that newly built schema to be converted
        
        // If the subschema is just bool then we can just return Any or None
        match subschema.BoolValue |> Option.ofNullable with
        | Some false -> None
        | Some true -> TypeSpec.Any None |> Conversion.Type |> Some
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
            sprintf "Needs more translation %O" allKeywords
            |> Some |> TypeSpec.Any |> Conversion.Type |> Some
        else 
            convertSubSchema root path context (newSchema.Build())

and convertArraySchema (root : RootInformation) path (jsonSchema : Json.Schema.JsonSchema) : Conversion option =    
    let prefixItems =
        jsonSchema.Keywords
        |> pickKeyword<Json.Schema.PrefixItemsKeyword>
        |> Option.map (fun kw -> 
            kw.ArraySchemas
            |> Seq.mapi (fun i s -> convertSubSchema root (sprintf "item%d" (i+1) :: path) ConversionContext.Default s)
        )

    match prefixItems with
    | Some items when Seq.exists Option.isNone items -> 
        // One of the tuple types is `false` so this tuple can't ever be constructed, so this object can't be constructed
        None
    | _ ->

    let items =
        jsonSchema.Keywords
        |> pickKeyword<Json.Schema.ItemsKeyword>
        |> Option.map (fun ik -> convertSubSchema root ("item" :: path) ConversionContext.Default ik.SingleSchema)
        |> Option.defaultValue (Some (Conversion.Type (TypeSpec.Any None)))

    let description = getDescription jsonSchema.Keywords
    match prefixItems, items with 
    | None, None -> 
        // Forced empty array, just make an array of any but nothing can go in it
        {
            Description = description
            Items = Conversion.Type (TypeSpec.Any None)
        } |> TypeSpec.Array |> Conversion.Type
    | Some prefixItems, None -> 
        // Tuple type!
        let prefixItems = prefixItems |> Seq.map Option.get |> Seq.toList
        {
            Path = path
            Title = getTitle jsonSchema.Keywords
            Description = description
            PrefixItems = prefixItems
            AdditionalItems = None
        } |> ComplexTypeSpec.Tuple |> Conversion.ComplexType
    | None, Some items ->
        // Basic array
        {
            Description = getDescription jsonSchema.Keywords
            Items = items
        } |> TypeSpec.Array |> Conversion.Type
    | Some prefixItems, Some items -> 
        // A tuple with rest
        let prefixItems = prefixItems |> Seq.map Option.get |> Seq.toList
        {
            Path = path
            Title = getTitle jsonSchema.Keywords
            Description = description
            PrefixItems = prefixItems
            AdditionalItems = Some items
        } |> ComplexTypeSpec.Tuple |> Conversion.ComplexType
    |> Some

and convertObjectSchema (root : RootInformation) path (context : ConversionContext) (jsonSchema : Json.Schema.JsonSchema) : Conversion =
    let propertiesKeyword = jsonSchema.Keywords |> pickKeyword<Json.Schema.PropertiesKeyword>
    let additionalPropertiesKeyword = jsonSchema.Keywords |> pickKeyword<Json.Schema.AdditionalPropertiesKeyword>
    let requiredKeyword = jsonSchema.Keywords |> pickKeyword<Json.Schema.RequiredKeyword>
    let unevaluatedPropertiesKeyword = jsonSchema.Keywords |> pickKeyword<Json.Schema.UnevaluatedPropertiesKeyword>

    let properties = 
        propertiesKeyword
        |> Option.map (fun pk ->
            pk.Properties
            |> Seq.choose (fun kv ->
                match convertSubSchema root (kv.Key :: path) ConversionContext.Default kv.Value  with 
                | Some subConversion -> Some (kv.Key, subConversion)
                | None -> None
            )
            |> Map.ofSeq
        )

    // Pulumi schema only directly supports maps (i.e additionalProperties is Some and properties = []) or fixed property bags (i.e. additionalProperties is None)
    // If we have both we need to add an extra "additionalProperties" property to the object
    let additionalProperties =
        match context.AdditionalProperties, additionalPropertiesKeyword with 
        | true, Some apk -> convertSubSchema root ("additionalProperties" :: path) ConversionContext.Default apk.Schema
        | true, None -> Some (Conversion.Type (TypeSpec.Any None))
        | false, _ -> None

    let required = 
        match requiredKeyword with
        | None -> Seq.empty
        | Some required -> required.Properties
        |> Set.ofSeq 
        
    let description = getDescription jsonSchema.Keywords
    let title = getTitle jsonSchema.Keywords

    match properties, additionalProperties with 
    | None, None -> 
        // An empty object!
        {
            Path = path
            Description = description
            Title = title
            Properties = Map.empty
            AdditionalProperties = None
            Required = required
            Choices = []
        }
        |> ComplexTypeSpec.Object
        |> Conversion.ComplexType
    | None, Some aps -> 
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

and convertAllOf (root : RootInformation) (path : string list) context (schema : Json.Schema.JsonSchema) (allOf : Json.Schema.AllOfKeyword): Conversion option =

    // allOf is a union of all the subschemas
    // we build a new _schema_ that merges the subschemas in ways that are possible to express in the type system (falling back to just any if we can't merge them)
    // then we send that newly built schema to be converted
    let boolSchemas, keywordSchemas = schema :: Seq.toList allOf.Schemas |> List.partition (fun s -> s.BoolValue.HasValue)

    // If _any_ are false we can early out
    match boolSchemas |> List.map (fun s -> s.BoolValue.Value) |> List.exists not with 
    | true -> None
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

    convertSubSchema root path context (newSchema.Build()) 

and convertOneOf (root : RootInformation) path context (schema : Json.Schema.JsonSchema) (oneOf : Json.Schema.OneOfKeyword) : Conversion option =

    // See if there's any validation keywords at this level or if it's just a simple oneOf
    let simpleOneOf = 
        schema.Keywords 
        |> Seq.forall (fun kw ->
            kw.Equals oneOf || not (isValidationKeyword kw)
        )

    let subConversions = 
        oneOf.Schemas 
        |> Seq.mapi (fun i subschema -> 
            let path = sprintf "oneOf%d" i :: path
            convertSubSchema root path context subschema)
        |> Seq.choose id
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
            ) (Some { Description = None; BooleanConversion = None; NumberConversion = None; StringConversion = None })

    match simpleOneOf, simpleUnion with
    | _, Some union -> union |> TypeSpec.Union |> Conversion.Type |> Some
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
        |> Some
    | false, _ -> 
        // This is a complex object. First try to translate this object _without_ the oneOf
        let newSchema = Json.Schema.JsonSchemaBuilder()
        schema.Keywords 
        |> Seq.iter (fun keyword ->
            if keyword.Equals oneOf then ()
            else newSchema.Add keyword
        )
        let obj = convertSubSchema root path context (newSchema.Build()) 

        match obj with 
        | Some (Conversion.ComplexType (ComplexTypeSpec.Object obj)) ->
            // this _should_ be an object now, 
            let subConversions = 
                oneOf.Schemas 
                |> Seq.mapi (fun i subschema -> 
                    let path = sprintf "oneOf%d" i :: path
                    convertSubSchema root path { context with AdditionalProperties = false } subschema)
                |> Seq.choose id
                |> Seq.toList

            { obj with Choices = subConversions }
            |> ComplexTypeSpec.Object
            |> Conversion.ComplexType
            |> Some
        | root -> 
            sprintf "unhandled root conversion for oneOf: %A" root
            |> Some
            |> TypeSpec.Any 
            |> Conversion.Type
            |> Some


and convertSubSchema (root : RootInformation) (path : string list) (context : ConversionContext) (schema : Json.Schema.JsonSchema)  : Conversion option =
    match schema.BoolValue |> Option.ofNullable with
    | Some false -> None
    | Some true -> TypeSpec.Any None |> Conversion.Type |> Some
    | None ->
        let keywords = schema.Keywords

        // If there are no validation keywords this is a simple any
        match keywords |> Seq.forall (isValidationKeyword >> not) with 
        | true -> 
            TypeSpec.Any (getDescription keywords)
            |> Conversion.Type
            |> Some
        | false -> 

        let typ = 
            match getType keywords, context.Type with 
            | None, None -> None
            | Some t, None -> Some t
            | None, Some t -> Some t
            | Some a, Some b -> Some (a ||| b)
        let context = { Type = typ; AdditionalProperties = context.AdditionalProperties }

        let allOf = pickKeyword<Json.Schema.AllOfKeyword> keywords
        let oneOf = pickKeyword<Json.Schema.OneOfKeyword> keywords
        let ref = pickKeyword<Json.Schema.RefKeyword> keywords
        let constKeyword = pickKeyword<Json.Schema.ConstKeyword> keywords

        let isSimpleType c =
            match typ with
            | Some t -> t = c
            | None -> false
        
        if constKeyword.IsSome then 
            convertConst root path schema constKeyword.Value |> Some
        elif ref.IsSome then
            convertRef root path context schema ref.Value
        elif allOf.IsSome then
            convertAllOf root path context schema allOf.Value
        elif oneOf.IsSome then
            convertOneOf root path context schema oneOf.Value
        elif isSimpleType Json.Schema.SchemaValueType.Null then 
            convertNullSchema schema |> TypeSpec.Null |> Conversion.Type |> Some
        elif isSimpleType Json.Schema.SchemaValueType.Boolean then 
            convertBoolSchema schema |> TypeSpec.Primitive |> Conversion.Type |> Some
        elif isSimpleType Json.Schema.SchemaValueType.String then 
            convertStringSchema path schema |> Some
        elif isSimpleType Json.Schema.SchemaValueType.Integer then 
            convertNumberSchema true schema |> TypeSpec.Primitive |> Conversion.Type |> Some
        elif isSimpleType Json.Schema.SchemaValueType.Number then 
            convertNumberSchema false schema |> TypeSpec.Primitive |> Conversion.Type |> Some
        elif isSimpleType Json.Schema.SchemaValueType.Object then 
            convertObjectSchema root path context schema |> Some
        elif isSimpleType Json.Schema.SchemaValueType.Array then 
            convertArraySchema root path schema
        elif isSimpleUnion keywords then
            convertSimpleUnion schema |> TypeSpec.Union |> Conversion.Type |> Some
        else 
            let msg =
                keywords
                |> Seq.map (fun kw -> kw.ToString())
                |> String.concat ", "
                |> sprintf "unhandled schema: %s"
            
            TypeSpec.Any (Some msg)
            |> Conversion.Type
            |> Some

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
    let conversion = convertSubSchema root [] { Type = None; AdditionalProperties = true } jsonSchema 

    let conversion = 
        match conversion with 
        | None -> failwith "top level false schemas are not supported, this schema can not read or write anything"
        | Some conversion -> conversion

    // We need to get all complex types and make names for them, then ask for the root schema to generate a pulumi schema for itself _given_ those names
    let complexTypes = conversion.CollectComplexTypes()
    let usedNames = System.Collections.Generic.HashSet()
    let names = System.Collections.Generic.Dictionary()
    for complexType in complexTypes do
        let name = 
            // If we have a title use that
            complexType.Title
            |> Option.map cleanTextForName
            |> Option.defaultWith (fun () ->
                // Else use the path of the type
                complexType.Path
                |> List.rev
                |> String.concat "_"
                |> cleanTextForName
            )
            // Default the empty string to "root"
            |> fun name -> if name = "" then "root" else name

        if usedNames.Contains name then
            failwith "Name conflicts not yet auto resolved"

        usedNames.Add(name) |> ignore
        names.Add(complexType, name)
            
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

    let validationOptions = Json.Schema.ValidationOptions()
    validationOptions.OutputFormat <- Json.Schema.OutputFormat.Basic
    
    let writer (value : Pulumi.Provider.PropertyValue) = 
        let result = conversion.Writer value
        let validation = jsonSchema.Validate(Option.toObj result, validationOptions)
        if not validation.IsValid then 
            failwith validation.Message
        else 
            result

    let reader (element : JsonElement) =
        let node = Json.More.JsonElementExtensions.AsNode(element)
        let validation = jsonSchema.Validate(node, validationOptions)
        if not validation.IsValid then 
            failwith validation.Message
        else 
            match conversion.Reader element with 
            | Ok ok -> ok
            | Error msg -> failwith msg

    {
        Schema = schema
        Reader = reader
        Writer = writer
    }

type Provider(conversion : RootConversion, host : Pulumi.Provider.IHost) =
    inherit Pulumi.Provider.Provider()

    override this.GetSchema(request : Pulumi.Provider.GetSchemaRequest, cancellationToken : System.Threading.CancellationToken) = 
        async {
            let resp = Pulumi.Provider.GetSchemaResponse()
            resp.Schema <- conversion.Schema.ToJsonString()
            return resp
        } 
        |> fun computation -> Async.StartAsTask(computation, Threading.Tasks.TaskCreationOptions.None, cancellationToken)

    override this.Invoke(request : Pulumi.Provider.InvokeRequest, cancellationToken : System.Threading.CancellationToken) = 
        async {
            let resp = Pulumi.Provider.InvokeResponse()
            if request.Tok = "jsonschema:index:read" then
                match request.Args.TryGetValue "json" with
                | false, _ -> failwith "Expected an input property 'json'"
                | true, jsonProperty ->
                    match jsonProperty.TryGetString() with 
                    | false, _ -> failwith "Expected input property 'json' to be a string"
                    | true, jsonText ->
                        let jsonData = System.Text.Encoding.UTF8.GetBytes jsonText
                        let mutable reader = Utf8JsonReader(jsonData)
                        let jsonElement = JsonElement.ParseValue(&reader)
                        let result = conversion.Reader jsonElement
                        resp.Return <- dict ["value", result]
            elif request.Tok = "jsonschema:index:write" then
                match request.Args.TryGetValue "value" with
                | false, _ -> failwith "Expected an input property 'value'"
                | true, valueProperty ->
                    let jsonNode = conversion.Writer valueProperty
                    match jsonNode with
                    | None -> resp.Return <- dict ["value", Pulumi.Provider.PropertyValue("null")]
                    | Some node -> resp.Return <- dict ["value", Pulumi.Provider.PropertyValue(node.ToJsonString())]
            else
                failwithf "Unknown invoke '%s'" request.Tok
            return resp
        } 
        |> fun computation -> Async.StartAsTask(computation, Threading.Tasks.TaskCreationOptions.None, cancellationToken)

[<EntryPoint>]
let main args =
    use cts = new System.Threading.CancellationTokenSource()
    let uri, schema = 
        if args.Length > 2 then 
            let contents = System.IO.File.ReadAllBytes(args[1])
            Uri(System.IO.Path.GetFullPath(args[1])), System.Text.Json.JsonDocument.Parse(contents)
        else 
            // Default to the pulumi schema to help with testing
            use client = new System.Net.Http.HttpClient()
            let uri = Uri("https://raw.githubusercontent.com/pulumi/pulumi/master/pkg/codegen/schema/pulumi.json")
            let contents = client.GetStringAsync(uri)
            uri, System.Text.Json.JsonDocument.Parse(contents.Result)
    let conversion = convertSchema uri schema.RootElement
    let task = Pulumi.Provider.Provider.Serve(args, (fun host -> Provider(conversion, host)), cts.Token)
    task.Wait()
    0