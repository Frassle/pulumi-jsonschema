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

[<RequireQualifiedAccess>]
type PrimitiveType = Boolean | Integer | Number | String
with 
    member this.JsonValue = 
        match this with 
        | Boolean -> JsonValue.Create("boolean")
        | Integer -> JsonValue.Create("integer")
        | Number -> JsonValue.Create("number")
        | String -> JsonValue.Create("string")

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
            Pulumi.Provider.PropertyValue.Null
        else 
            failwithf "Invalid JSON document expected null got %O" value.ValueKind

let writePrimitive (typ : PrimitiveType) (value : Pulumi.Provider.PropertyValue) = 
    let rec getNode (value : Pulumi.Provider.PropertyValue) =
        match typ with 
        | PrimitiveType.Boolean ->
            let raise  (typ : string) = failwithf "Invalid type expected boolean got %s" typ
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
        | PrimitiveType.Integer
        | PrimitiveType.Number ->
            let raise  (typ : string) = failwithf "Invalid type expected number got %s" typ
            value.Match(
                (fun _ -> raise "null"),
                (fun _ -> raise "bool"),
                (fun n -> 
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
            let raise  (typ : string) = failwithf "Invalid type expected string got %s" typ
            value.Match(
                (fun _ -> raise "null"),
                (fun _ -> raise "bool"),
                (fun _ -> raise "number"),
                (fun s -> 
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

let readPrimitive (typ: PrimitiveType) (value : JsonElement) =    
    match typ with 
    | PrimitiveType.Boolean ->
        if value.ValueKind = JsonValueKind.True then
            Pulumi.Provider.PropertyValue(true)
        elif value.ValueKind = JsonValueKind.False then
            Pulumi.Provider.PropertyValue(false)
        else 
            failwithf "Invalid JSON document expected bool got %O" value.ValueKind
    | PrimitiveType.Integer
    | PrimitiveType.Number ->
        if value.ValueKind = JsonValueKind.Number then
            Pulumi.Provider.PropertyValue(value.GetDouble())
        else 
            failwithf "Invalid JSON document expected number got %O" value.ValueKind
    | PrimitiveType.String ->
        if value.ValueKind = JsonValueKind.String then
            Pulumi.Provider.PropertyValue(value.GetString())
        else 
            failwithf "Invalid JSON document expected number got %O" value.ValueKind

type PrimitiveConversion = {
    Description : string option
    Type : PrimitiveType
} with
    member this.BuildTypeSpec() = 
        let schema = JsonObject()
        schema.Add("type", this.Type.JsonValue)
        schema, this.Description

    member this.BuildPropertySpec() =
        let schema = JsonObject()
        schema.Add("type", this.Type.JsonValue)
        this.Description |> Option.iter (fun desc -> schema.Add("description", desc))
        schema

    member this.Writer (value : Pulumi.Provider.PropertyValue) = 
        writePrimitive this.Type value

    member this.Reader (value : JsonElement) = 
        readPrimitive this.Type value

type UnionConversion = {
    Description : string option
    BooleanConversion : PrimitiveConversion option
    NumberConversion : PrimitiveConversion option
    StringConversion : PrimitiveConversion option
} with
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
                let value = v.Deserialize<JsonNode>()
                JsonObject([KeyValuePair.Create("value", value)]) :> JsonNode
            )
            |> Seq.toArray
            |> JsonArray
        schema.Add("enum", values)
        this.Description |> Option.iter (fun desc -> schema.Add("description", desc))
        schema

    member this.Writer (value : Pulumi.Provider.PropertyValue) = 
        writePrimitive this.Type value

    member this.Reader (value : JsonElement) = 
        readPrimitive this.Type value
                
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
            |> ImmutableArray.CreateRange
            |> Pulumi.Provider.PropertyValue
        else 
            failwithf "Invalid JSON document expected array got %O" value.ValueKind

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
            |> Seq.map (fun kv -> KeyValuePair.Create(kv.Name, this.AdditionalProperties.Reader kv.Value))
            |> ImmutableDictionary.CreateRange
            |> Pulumi.Provider.PropertyValue
        else 
            failwithf "Invalid JSON document expected object got %O" value.ValueKind

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
                Pulumi.Provider.PropertyValue.Null
            elif value.ValueKind = JsonValueKind.False then 
                Pulumi.Provider.PropertyValue(false)
            elif value.ValueKind = JsonValueKind.True then 
                Pulumi.Provider.PropertyValue(true)
            elif value.ValueKind = JsonValueKind.Number then 
                Pulumi.Provider.PropertyValue(value.GetDouble())
            elif value.ValueKind = JsonValueKind.String then 
                Pulumi.Provider.PropertyValue(value.GetString())
            elif value.ValueKind = JsonValueKind.Array then 
                value.EnumerateArray()
                |> Seq.map (fun item -> this.Reader item)
                |> ImmutableArray.CreateRange
                |> Pulumi.Provider.PropertyValue
            elif value.ValueKind = JsonValueKind.Object then 
                value.EnumerateObject()
                |> Seq.map (fun item -> KeyValuePair.Create(item.Name, this.Reader item.Value))
                |> ImmutableDictionary.CreateRange
                |> Pulumi.Provider.PropertyValue
            else 
                failwithf "unexpected JsonValueKind: %O" value.ValueKind
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

and ObjectConversion = {
    Path : string list
    Title : string option
    Description : string option
    Properties : Map<string, Conversion>
    AdditionalProperties : Conversion option
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
        for kv in this.Properties do
            propertiesSchema.Add(kv.Key, kv.Value.BuildPropertySpec packageName names)
        schema.Add("properties", propertiesSchema)

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
                    | Some conversion ->
                        let node = conversion.Writer kv.Value
                        match node with 
                        | Some node -> [KeyValuePair.Create(kv.Key, node)] :> seq<_>
                        | None -> [KeyValuePair.Create(kv.Key, null)]
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
                        | Some conversion ->
                            let node = conversion.Writer kv.Value
                            match node with 
                            | Some node -> [KeyValuePair.Create(kv.Key, node)]
                            | None -> [KeyValuePair.Create(kv.Key, null)]
                        | None -> failwithf "unexpected property '%s'" kv.Key
            )
            |> JsonObject
            :> JsonNode
            |> Some

    member this.Reader (value : JsonElement) =
        if value.ValueKind = JsonValueKind.Object then            
            let properties, additionalProperties =
                value.EnumerateObject()
                |> Seq.fold (fun (propsSoFar, addPropsSoFar) kv ->
                    match this.AdditionalProperties with
                    | None -> 
                        match this.Properties.TryFind kv.Name with 
                        | Some conversion ->
                            KeyValuePair.Create(kv.Name, conversion.Reader kv.Value) :: propsSoFar, []
                        | None -> failwithf "unexpected property '%s'" kv.Name
                    | Some additionalProperties ->
                        match this.Properties.TryFind kv.Name with
                        | Some conversion ->
                            KeyValuePair.Create(kv.Name, conversion.Reader kv.Value) :: propsSoFar, addPropsSoFar
                        | None ->                         
                            propsSoFar, KeyValuePair.Create(kv.Name, additionalProperties.Reader kv.Value) :: addPropsSoFar
                ) ([], [])

            // If we have any additionalProperties add them to the properties
            let properties = 
                match additionalProperties with
                | [] -> properties
                | additionalProperties ->
                    let arr = Pulumi.Provider.PropertyValue(ImmutableDictionary.CreateRange additionalProperties)
                    KeyValuePair.Create("additionalProperties", arr) ::  properties

            properties
            |> ImmutableDictionary.CreateRange
            |> Pulumi.Provider.PropertyValue
        else 
            failwithf "Invalid JSON document expected object got %O" value.ValueKind

    member this.CollectComplexTypes() : ImmutableHashSet<ComplexTypeSpec> =
        let complexTypes = 
            this.Properties
            |> Seq.fold (fun types kv -> 
                kv.Value.CollectComplexTypes().Union(types)
            ) ImmutableHashSet.Empty
        match this.AdditionalProperties with
        | None -> complexTypes
        | Some aps -> aps.CollectComplexTypes().Union(complexTypes)

        
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
            propertiesSchema.Add("additionalItems", ais.BuildPropertySpec packageName names)

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
            let properties, rest =
                value.EnumerateArray()
                |> Seq.mapi (fun i item -> (i, item))
                |> Seq.fold (fun (properties : Map<_,_>, rest) (i, item) ->
                    match List.tryItem i this.PrefixItems, this.AdditionalItems with 
                    | None, None -> failwithf "unexpected item %d in tuple" i
                    | None, Some ais -> properties, ais.Reader item :: rest
                    | Some property, _ -> properties.Add(i+1, property.Reader item), rest
                ) (Map.empty, [])
                
            // If we have any additionalItems we'll add them to the properties
            let rest = 
                match this.AdditionalItems, rest with 
                | Some _, rest -> 
                    let arr = rest |> List.rev |> ImmutableArray.CreateRange
                    Some (KeyValuePair.Create("rest", Pulumi.Provider.PropertyValue(arr)))
                | None, [] -> None
                | None, rest -> failwith "AdditionalItems isn't set, but we got values in rest"

            let properties = 
                properties
                |> Seq.map (fun kv -> 
                    KeyValuePair.Create(sprintf "item%d" kv.Key, kv.Value)
                )
                |> match rest with | None -> id | Some rest -> Seq.append [rest]

            properties
            |> ImmutableDictionary.CreateRange
            |> Pulumi.Provider.PropertyValue
        else 
            failwithf "Invalid JSON document expected array got %O" value.ValueKind

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
    | Tuple of TupleConversion

    member this.Path = 
        match this with 
        | ComplexTypeSpec.Enum spec -> spec.Path
        | ComplexTypeSpec.Object spec -> spec.Path
        | ComplexTypeSpec.Tuple spec -> spec.Path
    
    member this.Title = 
        match this with 
        | ComplexTypeSpec.Enum spec -> spec.Title
        | ComplexTypeSpec.Object spec -> spec.Title
        | ComplexTypeSpec.Tuple spec -> spec.Title
    
    member this.BuildComplexTypeSpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) =
        match this with 
        | ComplexTypeSpec.Enum spec -> spec.BuildComplexTypeSpec()
        | ComplexTypeSpec.Object spec -> spec.BuildComplexTypeSpec packageName names
        | ComplexTypeSpec.Tuple spec -> spec.BuildComplexTypeSpec packageName names

    member this.Writer (value : Pulumi.Provider.PropertyValue) =
        match this with 
        | ComplexTypeSpec.Enum spec -> spec.Writer value
        | ComplexTypeSpec.Object spec -> spec.Writer value
        | ComplexTypeSpec.Tuple spec -> spec.Writer value

    member this.Reader (value : JsonElement) =
        match this with 
        | ComplexTypeSpec.Enum spec -> spec.Reader value
        | ComplexTypeSpec.Object spec -> spec.Reader value
        | ComplexTypeSpec.Tuple spec -> spec.Reader value

    member this.CollectComplexTypes() : ImmutableHashSet<ComplexTypeSpec> =
        match this with 
        | ComplexTypeSpec.Enum _ -> ImmutableHashSet.Create(this)
        | ComplexTypeSpec.Object spec -> spec.CollectComplexTypes().Add(this)
        | ComplexTypeSpec.Tuple spec -> spec.CollectComplexTypes().Add(this)

and [<RequireQualifiedAccess>] Conversion =
    | TypeSpec of TypeSpec
    | ComplexTypeSpec of ComplexTypeSpec
         
    member this.BuildTypeSpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) =
        match this with 
        | Conversion.TypeSpec spec -> spec.BuildTypeSpec packageName names
        | Conversion.ComplexTypeSpec spec ->
            match names.TryGetValue spec with 
            | false, _ -> failwithf "Could not find name for %O" spec
            | true, name ->
                let schema = JsonObject()
                schema.Add("$ref", JsonValue.Create("#/types/" + packageName + ":index:" + name))
                schema, None

    member this.BuildPropertySpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) =
        match this with 
        | Conversion.TypeSpec spec -> spec.BuildPropertySpec packageName names
        | Conversion.ComplexTypeSpec spec ->
            match names.TryGetValue spec with 
            | false, _ -> failwithf "Could not find name for %O" spec
            | true, name ->
                let schema = JsonObject()
                schema.Add("$ref", JsonValue.Create("#/types/" + packageName + ":index:" + name))
                schema

    member this.Writer (value : Pulumi.Provider.PropertyValue) : JsonNode option =
        match this with 
        | Conversion.TypeSpec spec -> spec.Writer value
        | Conversion.ComplexTypeSpec spec -> spec.Writer value

    member this.Reader (value : JsonElement) : Pulumi.Provider.PropertyValue =
        match this with 
        | Conversion.TypeSpec spec -> spec.Reader value
        | Conversion.ComplexTypeSpec spec -> spec.Reader value

    member this.CollectComplexTypes() : ImmutableHashSet<ComplexTypeSpec> =
        match this with 
        | Conversion.TypeSpec spec -> spec.CollectComplexTypes()
        | Conversion.ComplexTypeSpec spec -> spec.CollectComplexTypes()

type RootInformation = {
    Uri : Uri
    Document : JsonElement
}

let isSimpleType (schemaValueType : Json.Schema.SchemaValueType) (keywords : KeywordCollection) : bool =
    keywords
    |> pickKeyword<Json.Schema.TypeKeyword>
    |> Option.map (fun typ ->
        typ.Type = schemaValueType
    ) |> Option.defaultValue false

let isSimpleUnion (keywords : KeywordCollection) : bool =
    keywords
    |> pickKeyword<Json.Schema.TypeKeyword>
    |> Option.map (fun typ ->
        not (typ.Type.HasFlag Json.Schema.SchemaValueType.Object) &&
        not (typ.Type.HasFlag Json.Schema.SchemaValueType.Array)
    ) |> Option.defaultValue false
        
let getDescription keywords = 
    match pickKeyword<Json.Schema.DescriptionKeyword> keywords with 
    | Some desc -> Some desc.Value
    | None -> None

let getTitle keywords = 
    match pickKeyword<Json.Schema.TitleKeyword> keywords with 
    | Some title -> Some title.Value
    | None -> None

let convertNullSchema (jsonSchema : Json.Schema.JsonSchema) : NullConversion =
    { 
        Description = getDescription jsonSchema.Keywords
    }

let convertBoolSchema (jsonSchema : Json.Schema.JsonSchema) : PrimitiveConversion =
    {
        Type = PrimitiveType.Boolean
        Description = getDescription jsonSchema.Keywords
    }

let convertStringSchema path (jsonSchema : Json.Schema.JsonSchema) : Conversion =
    let enum = pickKeyword<Json.Schema.EnumKeyword> jsonSchema.Keywords
    match enum with 
    | None -> 
        {
            Type = PrimitiveType.String
            Description = getDescription jsonSchema.Keywords
        }
        |> TypeSpec.Primitive
        |> Conversion.TypeSpec
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
        |> Conversion.ComplexTypeSpec

let convertNumberSchema (jsonSchema : Json.Schema.JsonSchema) : PrimitiveConversion =
    {
        Type = PrimitiveType.Number
        Description = getDescription jsonSchema.Keywords
    }

let convertSimpleUnion (jsonSchema : Json.Schema.JsonSchema) : UnionConversion =
    let typ = jsonSchema.Keywords |> pickKeyword<Json.Schema.TypeKeyword> |> Option.get
    
    let numberConversion =
        if typ.Type.HasFlag Json.Schema.SchemaValueType.Number then
            Some (convertNumberSchema jsonSchema)
        else None

    let stringConversion =
        if typ.Type.HasFlag Json.Schema.SchemaValueType.String then
            match convertStringSchema [] jsonSchema with 
            | Conversion.TypeSpec spec -> 
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
    
let rec convertRef (root : RootInformation) (ref : Json.Schema.RefKeyword) : Conversion option =
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

    let newUri = Uri(root.Uri, ref.Reference)
    match Json.Pointer.JsonPointer.TryParse newUri.Fragment with 
    | true, pointerFragment -> 
        match pointerFragment.Evaluate(root.Document) |> Option.ofNullable with
        | None -> failwithf "failed to find $ref %O" ref.Reference
        | Some subelement ->
            let subschema = Json.Schema.JsonSchema.FromText(subelement.GetRawText())
            // We need a new path here because this _ref_ could be seen by multiple paths
            let path = 
                pointerFragment.Segments
                |> Seq.map (fun s -> s.Value)
                |> Seq.toList
                |> function
                // Trim "$defs" off
                | "$defs" :: path -> path
                | path -> path

            convertSubSchema root (List.rev path) subschema
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

and convertArraySchema (root : RootInformation) path (jsonSchema : Json.Schema.JsonSchema) : Conversion option =    
    let prefixItems =
        jsonSchema.Keywords
        |> pickKeyword<Json.Schema.PrefixItemsKeyword>
        |> Option.map (fun kw -> 
            kw.ArraySchemas
            |> Seq.mapi (fun i s -> convertSubSchema root (sprintf "item%d" (i+1) :: path) s)
        )

    match prefixItems with
    | Some items when Seq.exists Option.isNone items -> 
        // One of the tuple types is `false` so this tuple can't ever be constructed, so this object can't be constructed
        None
    | _ ->

    let items =
        jsonSchema.Keywords
        |> pickKeyword<Json.Schema.ItemsKeyword>
        |> Option.map (fun ik -> convertSubSchema root ("item" :: path) ik.SingleSchema)
        |> Option.defaultValue (Some (Conversion.TypeSpec (TypeSpec.Any None)))

    let description = getDescription jsonSchema.Keywords
    match prefixItems, items with 
    | None, None -> 
        // Forced empty array, just make an array of any but nothing can go in it
        {
            Description = description
            Items = Conversion.TypeSpec (TypeSpec.Any None)
        } |> TypeSpec.Array |> Conversion.TypeSpec
    | Some prefixItems, None -> 
        // Tuple type!
        let prefixItems = prefixItems |> Seq.map Option.get |> Seq.toList
        {
            Path = path
            Title = getTitle jsonSchema.Keywords
            Description = description
            PrefixItems = prefixItems
            AdditionalItems = None
        } |> ComplexTypeSpec.Tuple |> Conversion.ComplexTypeSpec
    | None, Some items ->
        // Basic array
        {
            Description = getDescription jsonSchema.Keywords
            Items = items
        } |> TypeSpec.Array |> Conversion.TypeSpec
    | Some prefixItems, Some items -> 
        // A tuple with rest
        let prefixItems = prefixItems |> Seq.map Option.get |> Seq.toList
        {
            Path = path
            Title = getTitle jsonSchema.Keywords
            Description = description
            PrefixItems = prefixItems
            AdditionalItems = Some items
        } |> ComplexTypeSpec.Tuple |> Conversion.ComplexTypeSpec
    |> Some

and convertObjectSchema (root : RootInformation) path (jsonSchema : Json.Schema.JsonSchema) : Conversion =
    let propertiesKeyword = jsonSchema.Keywords |> pickKeyword<Json.Schema.PropertiesKeyword>
    let additionalPropertiesKeyword = jsonSchema.Keywords |> pickKeyword<Json.Schema.AdditionalPropertiesKeyword>
    let requiredKeyword = jsonSchema.Keywords |> pickKeyword<Json.Schema.RequiredKeyword>
    let unevaluatedPropertiesKeyword = jsonSchema.Keywords |> pickKeyword<Json.Schema.UnevaluatedPropertiesKeyword>

    let properties = 
        propertiesKeyword
        |> Option.map (fun pk ->
            pk.Properties
            |> Seq.map (fun kv ->
                match convertSubSchema root (kv.Key :: path) kv.Value with 
                | Some subConversion -> (kv.Key, subConversion)
                | None -> failwith "false properties not yet implemented"
            )
            |> Map.ofSeq
        )

    // Pulumi schema only directly supports maps (i.e additionalProperties is Some and properties = []) or fixed property bags (i.e. additionalProperties is None)
    // If we have both we need to add an extra "additionalProperties" property to the object
    let additionalProperties =
        match additionalPropertiesKeyword with 
        | Some apk -> convertSubSchema root ("additionalProperties" :: path) apk.Schema
        | None -> Some (Conversion.TypeSpec (TypeSpec.Any None))

    let properties = 
        match additionalProperties, properties with
        | Some apk, Some properties -> 
            // Make a fresh map conversion, this is dumb but we just reconvert the apk schema to get the mapping schema
            let mappingSchema = Json.Schema.JsonSchemaBuilder()
            mappingSchema.Add (Json.Schema.TypeKeyword Json.Schema.SchemaValueType.Object)
            match additionalPropertiesKeyword with 
            | Some apk -> mappingSchema.Add (Json.Schema.AdditionalPropertiesKeyword apk.Schema)
            | None -> ()

            let mapConversion = convertObjectSchema root ("additionalProperties" :: path) (mappingSchema.Build())

            Some (properties.Add ("additionalProperties", mapConversion))
        | _, properties -> properties

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
        }
        |> ComplexTypeSpec.Object
        |> Conversion.ComplexTypeSpec
    | None, Some aps -> 
        // A map
        {
            Description = description
            AdditionalProperties = aps
        }
        |> TypeSpec.Map
        |> Conversion.TypeSpec
    | Some props, aps ->
        {
            Path = path
            Description = description
            Title = title
            Properties = props
            AdditionalProperties = aps
            Required = required
        }
        |> ComplexTypeSpec.Object
        |> Conversion.ComplexTypeSpec

and convertAllOf (root : RootInformation) (path : string list) (schema : Json.Schema.JsonSchema) (allOf : Json.Schema.AllOfKeyword): Conversion option =
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
        |> Map.filter (fun _ list -> 
            match list with 
            | (:? Json.Schema.PropertiesKeyword) :: _ -> 
                list 
                |> Seq.cast<Json.Schema.PropertiesKeyword>
                |> Seq.map (fun kw -> kw.Properties)
                |> Seq.fold (fun properties next ->
                    next 
                    |> Seq.fold (fun properties property -> 
                        match Map.tryFind property.Key properties with 
                        | None -> Map.add property.Key property.Value properties
                        | Some _ -> failwith "Property key conflict in allOf"
                    ) properties
                ) Map.empty
                |> fun properties -> 
                    newSchema.Add(Json.Schema.PropertiesKeyword(properties))
                false
            | _ -> true)

    if allKeywords.Count <> 0 then
        failwithf "Needs more translation %O" allKeywords

    convertSubSchema root path (newSchema.Build())

and convertOneOf (schema : Json.Schema.JsonSchema) (oneOf : Json.Schema.OneOfKeyword) : Conversion =
    TypeSpec.Any (Some "default any for anyOf")
    |> Conversion.TypeSpec

and convertSubSchema (root : RootInformation) (path : string list) (schema : Json.Schema.JsonSchema) : Conversion option =
    match schema.BoolValue |> Option.ofNullable with
    | Some false -> None
    | Some true -> TypeSpec.Any None |> Conversion.TypeSpec |> Some
    | None ->
        let keywords = schema.Keywords

        // If there are no validation keywords this is a simple any
        match keywords |> Seq.forall (isValidationKeyword >> not) with 
        | true -> 
            TypeSpec.Any (getDescription keywords)
            |> Conversion.TypeSpec
            |> Some
        | false -> 

        let allOf = pickKeyword<Json.Schema.AllOfKeyword> keywords
        let oneOf = pickKeyword<Json.Schema.OneOfKeyword> keywords
        let ref = pickKeyword<Json.Schema.RefKeyword> keywords
        
        if ref.IsSome then
            convertRef root ref.Value
        elif allOf.IsSome then
            convertAllOf root path schema allOf.Value
        elif oneOf.IsSome then
            Some (convertOneOf schema oneOf.Value)
        elif isSimpleType Json.Schema.SchemaValueType.Null keywords then 
            convertNullSchema schema |> TypeSpec.Null |> Conversion.TypeSpec |> Some
        elif isSimpleType Json.Schema.SchemaValueType.Boolean keywords then 
            convertBoolSchema schema |> TypeSpec.Primitive |> Conversion.TypeSpec |> Some
        elif isSimpleType Json.Schema.SchemaValueType.String keywords then 
            convertStringSchema path schema |> Some
        elif isSimpleType Json.Schema.SchemaValueType.Number keywords then 
            convertNumberSchema schema |> TypeSpec.Primitive |> Conversion.TypeSpec |> Some
        elif isSimpleType Json.Schema.SchemaValueType.Object keywords then 
            convertObjectSchema root path schema |> Some
        elif isSimpleType Json.Schema.SchemaValueType.Array keywords then 
            convertArraySchema root path schema
        elif isSimpleUnion keywords then
            convertSimpleUnion schema |> TypeSpec.Union |> Conversion.TypeSpec |> Some
        else 
            let msg =
                keywords
                |> Seq.map (fun kw -> kw.ToString())
                |> String.concat ", "
                |> sprintf "unhandled schema: %s"
            
            TypeSpec.Any (Some msg)
            |> Conversion.TypeSpec
            |> Some

type RootConversion = {
    Schema: JsonObject
    Writer : Pulumi.Provider.PropertyValue -> JsonNode option
    Reader : JsonElement ->  Pulumi.Provider.PropertyValue
}

let cleanTitleForTypeName (title : string) : string =
    // Just remove any invalid chars for now
    title.Replace(" ", "").Replace("-", "_")

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
        Uri = uri
        Document = jsonSchema
    }
    let jsonSchema = Json.Schema.JsonSchema.FromText (jsonSchema.GetRawText())
    let conversion = convertSubSchema root [] jsonSchema

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
            |> Option.map cleanTitleForTypeName
            |> Option.defaultWith (fun () ->
                // Else use the path of the type
                complexType.Path
                |> List.rev
                |> String.concat "_"
                |> cleanTitleForTypeName
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
        let node = element.Deserialize<JsonNode>()
        let validation = jsonSchema.Validate(node, validationOptions)
        if not validation.IsValid then 
            failwith validation.Message
        else 
            conversion.Reader element

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