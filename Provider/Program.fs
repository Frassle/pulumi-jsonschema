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

let validate (jsonSchema : Json.Schema.JsonSchema) (node : JsonNode option) : JsonNode option =
    let options = Json.Schema.ValidationOptions()
    options.OutputFormat <- Json.Schema.OutputFormat.Basic
    let validation = jsonSchema.Validate(Option.toObj node, options)
    if not validation.IsValid then 
        failwith validation.Message
    else 
        node

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

let writePrimitive (typ : PrimitiveType) (schema : Json.Schema.JsonSchema) (value : Pulumi.Provider.PropertyValue) = 
    let rec getNode (value : Pulumi.Provider.PropertyValue) =
        match typ with 
        | PrimitiveType.Boolean ->
            let raise  (typ : string) = failwithf "Invalid type expected bool got %s" typ
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
    validate schema (getNode value)

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
    Schema : Json.Schema.JsonSchema
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
        writePrimitive this.Type this.Schema value

    member this.Reader (value : JsonElement) = 
        readPrimitive this.Type value

type UnionConversion = {
    Schema : Json.Schema.JsonSchema
    Description : string option
    BooleanConversion : PrimitiveConversion option
    NumberConversion : PrimitiveConversion option
    StringConversion : PrimitiveConversion option
} with
    member this.BuildTypeSpec () =
        let schema = JsonObject()

        let oneof = JsonArray()
        [this.NumberConversion; this.BooleanConversion; this.StringConversion]
        |> List.iter (function 
            | None -> ()
            | Some conversion -> 
                let spec, _ = conversion.BuildTypeSpec()
                oneof.Add(spec))

        schema, this.Description

    member this.BuildPropertySpec () =
        let schema = JsonObject()

        let oneof = JsonArray()
        [this.NumberConversion; this.BooleanConversion; this.StringConversion]
        |> List.iter (function 
            | None -> ()
            | Some conversion -> 
                let spec, _ = conversion.BuildTypeSpec()
                oneof.Add(spec))
                
        this.Description |> Option.iter (fun desc -> schema.Add("description", desc))
        schema

    member private this.expectedTypes = 
        ["number", this.NumberConversion; "string", this.StringConversion; "boolean", this.BooleanConversion]
          |> List.choose (fun (k, opt) -> match opt with | Some _ -> Some k | None -> None)
          |> String.concat " or "      

    member this.Writer (value : Pulumi.Provider.PropertyValue) = 
        let raise (typ : string) = failwithf "Invalid type expected %s got %s" this.expectedTypes typ
        value.Match(
            (fun _ -> raise "null"),
            (fun _ -> 
                match this.BooleanConversion with 
                | None -> raise "bool"
                | Some conversion -> conversion.Writer value
            ),
            (fun _ -> 
                match this.BooleanConversion with 
                | None -> raise "number"
                | Some conversion -> conversion.Writer value
            ),
            (fun _ -> 
                match this.BooleanConversion with 
                | None -> raise "string"
                | Some conversion -> conversion.Writer value
            ),
            (fun _ -> raise "array"),
            (fun _ -> raise "object"),
            (fun _ -> raise "asset"),
            (fun _ -> raise "archive"),
            (fun secret -> this.Writer secret),
            (fun _ -> raise "resource"),            
            (fun output -> this.Writer output.Value),
            (fun _ -> raise "computed")
        )
        |> validate this.Schema

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
    Schema : Json.Schema.JsonSchema
    Description : string option
    Type : PrimitiveType
    Values : JsonNode list
} with        
    member this.BuildComplexTypeSpec() =
        let schema = JsonObject()
        schema.Add("type", this.Type.JsonValue)
        let values = 
            this.Values
            |> Seq.map (fun v -> JsonObject([KeyValuePair.Create("value", v)]) :> JsonNode)
            |> Seq.toArray
            |> JsonArray
        schema.Add("enum", values)
        this.Description |> Option.iter (fun desc -> schema.Add("description", desc))
        schema

    member this.Writer (value : Pulumi.Provider.PropertyValue) = 
        writePrimitive this.Type this.Schema value

    member this.Reader (value : JsonElement) = 
        readPrimitive this.Type value

                
type ArrayConversion = {
    Schema : Json.Schema.JsonSchema
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
        let raise (typ : string) = failwithf "Invalid type expected array got %s" typ
        value.Match(
            (fun _ -> raise "null"),
            (fun _ -> raise "bool"),
            (fun _ -> raise "number"),
            (fun _ -> raise "string"),
            (fun obj -> 
                obj
                |> Seq.map (fun item ->
                    this.Items.Writer item
                    |> Option.toObj
                )
                |> Seq.toArray
                |> JsonArray
                :> JsonNode
                |> Some
            ),
            (fun _ -> raise "object"),
            (fun _ -> raise "asset"),
            (fun _ -> raise "archive"),
            (fun secret -> this.Writer secret),
            (fun _ -> raise "resource"),            
            (fun output -> this.Writer output.Value),
            (fun _ -> raise "computed")
        )
        |> validate this.Schema

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
    Schema : Json.Schema.JsonSchema
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
        let raise (typ : string) = failwithf "Invalid type expected object got %s" typ
        value.Match(
            (fun _ -> raise "null"),
            (fun _ -> raise "bool"),
            (fun _ -> raise "number"),
            (fun _ -> raise "string"),
            (fun _ -> raise "array"),
            (fun obj ->
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
            ),
            (fun _ -> raise "asset"),
            (fun _ -> raise "archive"),
            (fun secret -> this.Writer secret),
            (fun _ -> raise "resource"),            
            (fun output -> this.Writer output.Value),
            (fun _ -> raise "computed")
        )
        |> validate this.Schema

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
    | Any of (Json.Schema.JsonSchema * string option) // Also known as Any
    | NullConversion of NullConversion
    | PrimitiveConversion of PrimitiveConversion
    | ArrayConversion of ArrayConversion
    | MapConversion of MapConversion
    | UnionConversion of UnionConversion

    member this.BuildTypeSpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) = 
        match this with 
        | Any (_, desc) ->
            let schema = JsonObject()
            schema.Add("$ref", JsonValue.Create("pulumi.json#/Any"))
            schema, desc
        | NullConversion c -> c.BuildTypeSpec()
        | PrimitiveConversion c -> c.BuildTypeSpec()
        | ArrayConversion c -> c.BuildTypeSpec packageName names
        | MapConversion c -> c.BuildTypeSpec packageName names
        | UnionConversion c -> c.BuildTypeSpec()

    member this.BuildPropertySpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) = 
        match this with 
        | Any (_, desc) ->
            let schema = JsonObject()
            schema.Add("$ref", JsonValue.Create("pulumi.json#/Any"))
            desc |> Option.iter (fun desc -> schema.Add("description", desc))
            schema
        | NullConversion c -> c.BuildPropertySpec()
        | PrimitiveConversion c -> c.BuildPropertySpec()
        | ArrayConversion c -> c.BuildPropertySpec packageName names
        | MapConversion c -> c.BuildPropertySpec packageName names
        | UnionConversion c -> c.BuildPropertySpec()
        
    member this.Writer (value : Pulumi.Provider.PropertyValue) = 
        match this with 
        | Any (schema, _) -> 
            let rec getValue (value : Pulumi.Provider.PropertyValue) =
                value.Match(
                    (fun () -> None),
                    (fun b -> JsonValue.Create(b) :> JsonNode |> Option.ofObj),
                    (fun n -> JsonValue.Create(n) :> JsonNode |> Option.ofObj),
                    (fun s -> JsonValue.Create(s) :> JsonNode |> Option.ofObj),
                    (fun a -> failwith "not implemented"),
                    (fun o -> failwith "not implemented"),
                    (fun _ -> failwith "not implemented"),
                    (fun _ -> failwith "not implemented"),
                    (fun secret -> getValue secret),
                    (fun _ -> failwith "not implemented"),   
                    (fun output -> getValue output.Value),
                    (fun _ ->failwith "not implemented")
                )
            validate schema (getValue value)
        | NullConversion c -> c.Writer value
        | PrimitiveConversion c -> c.Writer value
        | ArrayConversion c -> c.Writer value
        | MapConversion c -> c.Writer value        
        | UnionConversion c -> c.Writer value
        
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
        | NullConversion c -> c.Reader value
        | PrimitiveConversion c -> c.Reader value
        | ArrayConversion c -> c.Reader value
        | MapConversion c -> c.Reader value 
        | UnionConversion c -> c.Reader value

    member this.CollectComplexTypes() : ImmutableHashSet<ComplexTypeSpec> =
        match this with 
        | Any _ -> ImmutableHashSet.Empty
        | NullConversion c -> ImmutableHashSet.Empty
        | PrimitiveConversion c -> ImmutableHashSet.Empty
        | ArrayConversion c -> c.CollectComplexTypes()
        | MapConversion c -> c.CollectComplexTypes()
        | UnionConversion c -> ImmutableHashSet.Empty

and ObjectConversion = {
    Schema : Json.Schema.JsonSchema
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
        let raise (typ : string) = failwithf "Invalid type expected object got %s" typ
        value.Match(
            (fun _ -> raise "null"),
            (fun _ -> raise "bool"),
            (fun _ -> raise "number"),
            (fun _ -> raise "string"),
            (fun _ -> raise "array"),
            (fun obj ->
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
            ),
            (fun _ -> raise "asset"),
            (fun _ -> raise "archive"),
            (fun secret -> this.Writer secret),
            (fun _ -> raise "resource"),            
            (fun output -> this.Writer output.Value),
            (fun _ -> raise "computed")
        )
        |> validate this.Schema

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

and [<RequireQualifiedAccess>] ComplexTypeSpec =
    | Enum of EnumConversion
    | Object of ObjectConversion
    
    member this.BuildComplexTypeSpec packageName (names : ImmutableDictionary<ComplexTypeSpec, string>) =
        match this with 
        | ComplexTypeSpec.Enum spec -> spec.BuildComplexTypeSpec()
        | ComplexTypeSpec.Object spec -> spec.BuildComplexTypeSpec packageName names

    member this.Writer (value : Pulumi.Provider.PropertyValue) =
        match this with 
        | ComplexTypeSpec.Enum spec -> spec.Writer value
        | ComplexTypeSpec.Object spec -> spec.Writer value

    member this.Reader (value : JsonElement) =
        match this with 
        | ComplexTypeSpec.Enum spec -> spec.Reader value
        | ComplexTypeSpec.Object spec -> spec.Reader value

    member this.CollectComplexTypes() : ImmutableHashSet<ComplexTypeSpec> =
        match this with 
        | ComplexTypeSpec.Enum _ -> ImmutableHashSet.Create(this)
        | ComplexTypeSpec.Object spec -> spec.CollectComplexTypes().Add(this)

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
        
let description keywords = 
    match pickKeyword<Json.Schema.DescriptionKeyword> keywords with 
    | Some desc -> Some desc.Value
    | None -> None

let convertNullSchema (jsonSchema : Json.Schema.JsonSchema) : NullConversion =
    { 
        Description = description jsonSchema.Keywords
    }

let convertBoolSchema (jsonSchema : Json.Schema.JsonSchema) : PrimitiveConversion =
    {
        Schema = jsonSchema
        Type = PrimitiveType.Boolean
        Description = description jsonSchema.Keywords
    }

let convertStringSchema (jsonSchema : Json.Schema.JsonSchema) : Conversion =
    let enum = pickKeyword<Json.Schema.EnumKeyword> jsonSchema.Keywords
    match enum with 
    | None -> 
        {
            Schema = jsonSchema
            Type = PrimitiveType.String
            Description = description jsonSchema.Keywords
        }
        |> TypeSpec.PrimitiveConversion
        |> Conversion.TypeSpec
    | Some enum ->
        let enumValues =
            enum.Values
            |> Seq.toList

        {   
            Schema = jsonSchema
            Type = PrimitiveType.String
            Description = description jsonSchema.Keywords
            Values = enumValues
        }
        |> ComplexTypeSpec.Enum
        |> Conversion.ComplexTypeSpec

let convertNumberSchema (jsonSchema : Json.Schema.JsonSchema) : PrimitiveConversion =
    {
        Schema = jsonSchema
        Type = PrimitiveType.Number
        Description = description jsonSchema.Keywords
    }

let convertSimpleUnion (jsonSchema : Json.Schema.JsonSchema) : UnionConversion =
    let typ = jsonSchema.Keywords |> pickKeyword<Json.Schema.TypeKeyword> |> Option.get
    
    let numberConversion =
        if typ.Type.HasFlag Json.Schema.SchemaValueType.Number then
            Some (convertNumberSchema jsonSchema)
        else None

    let stringConversion =
        if typ.Type.HasFlag Json.Schema.SchemaValueType.String then
            match convertStringSchema jsonSchema with 
            | Conversion.TypeSpec spec -> 
                match spec with 
                | TypeSpec.PrimitiveConversion p -> Some p
                | _ -> failwith "Expected convertStringSchema to return a PrimitiveConversion"
            | _ -> failwith "Expected convertStringSchema to return a PrimitiveConversion"
        else None

    let boolConversion =
        if typ.Type.HasFlag Json.Schema.SchemaValueType.Boolean then
            Some (convertBoolSchema jsonSchema)
        else None

    {
        Schema = jsonSchema
        Description = description jsonSchema.Keywords
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
            convertSubSchema root subschema
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

and convertArraySchema (root : RootInformation) (jsonSchema : Json.Schema.JsonSchema) : ArrayConversion =
    let itemsKeyword = jsonSchema.Keywords |> pickKeyword<Json.Schema.ItemsKeyword>

    let items =
        itemsKeyword
        |> Option.map (fun ik -> convertSubSchema root ik.SingleSchema)
        |> Option.defaultValue (Some (Conversion.TypeSpec (TypeSpec.Any (Json.Schema.JsonSchema.True, None))))
        |> function  
        | None -> failwith "array with false items not yet supported"
        | Some items -> items

    {
        Schema = jsonSchema
        Description = description jsonSchema.Keywords
        Items = items
    }

and convertObjectSchema (root : RootInformation) (jsonSchema : Json.Schema.JsonSchema) : Conversion =
    let propertiesKeyword = jsonSchema.Keywords |> pickKeyword<Json.Schema.PropertiesKeyword>
    let additionalPropertiesKeyword = jsonSchema.Keywords |> pickKeyword<Json.Schema.AdditionalPropertiesKeyword>
    let requiredKeyword = jsonSchema.Keywords |> pickKeyword<Json.Schema.RequiredKeyword>
    let unevaluatedPropertiesKeyword = jsonSchema.Keywords |> pickKeyword<Json.Schema.UnevaluatedPropertiesKeyword>

    let properties = 
        propertiesKeyword
        |> Option.map (fun pk ->
            pk.Properties
            |> Seq.map (fun kv ->
                match convertSubSchema root kv.Value with 
                | Some subConversion -> (kv.Key, subConversion)
                | None -> failwith "false properties not yet implemented"
            )
            |> Map.ofSeq
        )

    // Pulumi schema only directly supports maps (i.e additionalProperties is Some and properties = []) or fixed property bags (i.e. additionalProperties is None)
    // If we have both we need to add an extra "additionalProperties" property to the object
    let additionalProperties =
        match additionalPropertiesKeyword with 
        | Some apk -> convertSubSchema root apk.Schema
        | None -> Some (Conversion.TypeSpec (TypeSpec.Any (Json.Schema.JsonSchema.True, None)))

    let properties = 
        match additionalProperties, properties with
        | Some apk, Some properties -> 
            // Make a fresh map conversion, this is dumb but we just reconvert the apk schema to get the mapping schema
            let mappingSchema = Json.Schema.JsonSchemaBuilder()
            mappingSchema.Add (Json.Schema.TypeKeyword Json.Schema.SchemaValueType.Object)
            match additionalPropertiesKeyword with 
            | Some apk -> mappingSchema.Add (Json.Schema.AdditionalPropertiesKeyword apk.Schema)
            | None -> ()

            let mapConversion = convertObjectSchema root (mappingSchema.Build())

            Some (properties.Add ("additionalProperties", mapConversion))
        | _, properties -> properties

    let required = 
        match requiredKeyword with
        | None -> Seq.empty
        | Some required -> required.Properties
        |> Set.ofSeq 

    let description = description jsonSchema.Keywords

    match properties, additionalProperties with 
    | None, None -> 
        // An empty object!
        {
            Schema = jsonSchema
            Description = description
            Properties = Map.empty
            AdditionalProperties = None
            Required = required
        }
        |> ComplexTypeSpec.Object
        |> Conversion.ComplexTypeSpec
    | None, Some aps -> 
        // A map
        {
            Schema = jsonSchema
            Description = description
            AdditionalProperties = aps
        }
        |> TypeSpec.MapConversion
        |> Conversion.TypeSpec
    | Some props, aps ->
        {
            Schema = jsonSchema
            Description = description
            Properties = props
            AdditionalProperties = aps
            Required = required
        }
        |> ComplexTypeSpec.Object
        |> Conversion.ComplexTypeSpec

and convertAllOf (schema : Json.Schema.JsonSchema) (allof : Json.Schema.AllOfKeyword) : Conversion =    
    TypeSpec.Any (schema, Some "default any for allOf")
    |> Conversion.TypeSpec

and convertOneOf (schema : Json.Schema.JsonSchema) (oneOf : Json.Schema.OneOfKeyword) : Conversion =
    TypeSpec.Any (schema, Some "default any for anyOf")
    |> Conversion.TypeSpec

and convertSubSchema (root : RootInformation) (schema : Json.Schema.JsonSchema) : Conversion option =
    match schema.BoolValue |> Option.ofNullable with
    | Some false -> None
    | Some true -> TypeSpec.Any (schema, None) |> Conversion.TypeSpec |> Some
    | None ->
        let keywords = schema.Keywords
        let allOf = pickKeyword<Json.Schema.AllOfKeyword> keywords
        let oneOf = pickKeyword<Json.Schema.OneOfKeyword> keywords
        let ref = pickKeyword<Json.Schema.RefKeyword> keywords
        
        if ref.IsSome then
            convertRef root ref.Value
        elif allOf.IsSome then
            Some (convertAllOf schema allOf.Value)
        elif oneOf.IsSome then
            Some (convertOneOf schema oneOf.Value)
        elif isSimpleType Json.Schema.SchemaValueType.Null keywords then 
            convertNullSchema schema |> TypeSpec.NullConversion |> Conversion.TypeSpec |> Some
        elif isSimpleType Json.Schema.SchemaValueType.Boolean keywords then 
            convertBoolSchema schema |> TypeSpec.PrimitiveConversion |> Conversion.TypeSpec |> Some
        elif isSimpleType Json.Schema.SchemaValueType.String keywords then 
            convertStringSchema schema |> Some
        elif isSimpleType Json.Schema.SchemaValueType.Number keywords then 
            convertNumberSchema schema |> TypeSpec.PrimitiveConversion |> Conversion.TypeSpec |> Some
        elif isSimpleType Json.Schema.SchemaValueType.Object keywords then 
            convertObjectSchema root schema |> Some
        elif isSimpleType Json.Schema.SchemaValueType.Array keywords then 
            convertArraySchema root schema |> TypeSpec.ArrayConversion |> Conversion.TypeSpec |> Some
        elif isSimpleUnion keywords then
            convertSimpleUnion schema |> TypeSpec.UnionConversion |> Conversion.TypeSpec |> Some
        else 
            let msg =
                keywords
                |> Seq.map (fun kw -> kw.ToString())
                |> String.concat ", "
                |> sprintf "unhandled schema: %s"
            
            TypeSpec.Any (schema, Some msg)
            |> Conversion.TypeSpec
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
        Uri = uri
        Document = jsonSchema
    }
    let jsonSchema = Json.Schema.JsonSchema.FromText (jsonSchema.GetRawText())
    let conversion = convertSubSchema root jsonSchema

    let conversion = 
        match conversion with 
        | None -> failwith "top level false schemas are not supported, this schema can not read or write anything"
        | Some conversion -> conversion

    // We need to get all complex types and make names for them, then ask for the root schema to generate a pulumi schema for itself _given_ those names
    let complexTypes = conversion.CollectComplexTypes()
    let names = System.Collections.Generic.Dictionary()
    let mutable namei = 0
    for complexType in complexTypes do
        let name = "a" + namei.ToString()
        namei <- namei + 1
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

    {
        Schema = schema
        Reader = conversion.Reader
        Writer = conversion.Writer
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