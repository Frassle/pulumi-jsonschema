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

type SchemaConversion = {
    Schema : JsonObject
    Writer : Pulumi.Provider.PropertyValue -> JsonNode option
    Reader : JsonElement ->  Pulumi.Provider.PropertyValue
    Description : string option
}

type Conversion =
    | Any of string option // Also known as Any
    | TypeSpec of SchemaConversion
    | ComplexTypeSpec of SchemaConversion

    member this.Description = 
        match this with 
        | Any desc -> desc
        | TypeSpec c -> c.Description
        | ComplexTypeSpec c -> c.Description

    member this.IsComplexType = 
        match this with 
        | ComplexTypeSpec _ -> true
        | _ -> false

    member this.Schema = 
        match this with 
        | Any _ ->
            let schema = JsonObject()
            schema.Add("$ref", JsonValue.Create("pulumi.json#/Any"))
            schema
        | TypeSpec conversion -> conversion.Schema
        | ComplexTypeSpec conversion -> conversion.Schema
        
    member this.Writer = 
        match this with 
        | Any _ -> 
            fun (value : Pulumi.Provider.PropertyValue) ->
                value.Match<JsonNode>(
                    (fun () -> null),
                    (fun b -> JsonValue.Create(b)),
                    (fun n -> JsonValue.Create(n)),
                    (fun s -> JsonValue.Create(s)),
                    (fun a -> failwith "not implemented"),
                    (fun o -> failwith "not implemented"),
                    (fun _ -> failwith "not implemented"),
                    (fun _ -> failwith "not implemented"),
                    (fun secret -> failwith "not implemented"),
                    (fun _ -> failwith "not implemented"),   
                    (fun output -> failwith "not implemented"),
                    (fun _ ->failwith "not implemented")
                ) |> Option.ofObj
        | TypeSpec conversion -> conversion.Writer
        | ComplexTypeSpec conversion -> conversion.Writer

    member this.Reader = 
        match this with 
        | Any _ -> 
            fun (value : JsonElement) ->
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
        | TypeSpec conversion -> conversion.Reader
        | ComplexTypeSpec conversion -> conversion.Reader

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

let wrapWriter  (jsonSchema : Json.Schema.JsonSchema) (writer :  Pulumi.Provider.PropertyValue -> JsonNode option) : Pulumi.Provider.PropertyValue -> JsonNode option =
    fun (value : Pulumi.Provider.PropertyValue) ->
        let jsonNode = writer value
        let options = Json.Schema.ValidationOptions()
        options.OutputFormat <- Json.Schema.OutputFormat.Basic
        let validation = jsonSchema.Validate(Option.toObj jsonNode, options)
        if not validation.IsValid then 
            failwith validation.Message
        else 
            jsonNode

let convertNullSchema (jsonSchema : Json.Schema.JsonSchema) : Conversion =
    let schema = JsonObject()
    schema.Add("$ref", JsonValue.Create("pulumi.json#/Any"))

    let raise  (typ : string) = failwithf "Invalid type expected null got %s" typ
    let rec writer (value : Pulumi.Provider.PropertyValue) =
        value.Match(
            (fun _ -> None),
            (fun _ -> raise "bool"),
            (fun _ -> raise "number"),
            (fun _ -> raise "string"),
            (fun _ -> raise "array"),
            (fun _ -> raise "object"),
            (fun _ -> raise "asset"),
            (fun _ -> raise "archive"),
            (fun secret -> writer secret),
            (fun _ -> raise "resource"),            
            (fun output -> writer output.Value),
            (fun _ -> raise "computed")
        )

    let reader  (value : JsonElement) = 
        if value.ValueKind = JsonValueKind.Null then
            Pulumi.Provider.PropertyValue.Null
        else 
            failwithf "Invalid JSON document expected null got %O" value.ValueKind

    TypeSpec {
        Description = description jsonSchema.Keywords
        Schema = schema
        Writer = wrapWriter jsonSchema writer
        Reader = reader
    }

let convertBoolSchema (jsonSchema : Json.Schema.JsonSchema) : Conversion =
    let schema = JsonObject()
    schema.Add("type", JsonValue.Create("bool"))

    let raise  (typ : string) = failwithf "Invalid type expected bool got %s" typ
    let rec writer (value : Pulumi.Provider.PropertyValue) =
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
            (fun secret -> writer secret),
            (fun _ -> raise "resource"),            
            (fun output -> writer output.Value),
            (fun _ -> raise "computed")
        )

    let reader  (value : JsonElement) = 
        if value.ValueKind = JsonValueKind.True then
            Pulumi.Provider.PropertyValue(true)
        elif value.ValueKind = JsonValueKind.False then
            Pulumi.Provider.PropertyValue(false)
        else 
            failwithf "Invalid JSON document expected bool got %O" value.ValueKind

    TypeSpec {
        Description = description jsonSchema.Keywords
        Schema = schema
        Writer = wrapWriter jsonSchema writer
        Reader = reader
    }

let convertStringSchema (jsonSchema : Json.Schema.JsonSchema) : Conversion =
    let schema = JsonObject()
    schema.Add("type", JsonValue.Create("string"))

    let raise (typ : string) = failwithf "Invalid type expected string got %s" typ
    let rec writer (value : Pulumi.Provider.PropertyValue) : JsonNode option =
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
            (fun secret -> writer secret),
            (fun _ -> raise "resource"),            
            (fun output -> writer output.Value),
            (fun _ -> raise "computed")
        )

    let reader (value : JsonElement) = 
        if value.ValueKind = JsonValueKind.String then
            Pulumi.Provider.PropertyValue(value.GetString())
        else 
            failwithf "Invalid JSON document expected string got %O" value.ValueKind            

    let enum = pickKeyword<Json.Schema.EnumKeyword> jsonSchema.Keywords
    match enum with 
    | None -> 
        TypeSpec {
            Description = description jsonSchema.Keywords
            Schema = schema
            Writer = wrapWriter jsonSchema writer
            Reader = reader
        }
    | Some enum ->
        let enumValues =
            enum.Values
            |> Seq.map (fun value -> 
                JsonObject([
                    KeyValuePair.Create("value", value.Deserialize<JsonNode>())
                ]) :> JsonNode
            )
            |> Seq.toArray
            |> JsonArray
        schema.Add("enum", enumValues)

        ComplexTypeSpec {
            Description = description jsonSchema.Keywords
            Schema = schema
            Writer = wrapWriter jsonSchema writer
            Reader = reader
        }


let convertNumberSchema (jsonSchema : Json.Schema.JsonSchema) : Conversion =
    let schema = JsonObject()
    schema.Add("type", JsonValue.Create("number"))

    let raise (typ : string) = failwithf "Invalid type expected number got %s" typ
    let rec writer (value : Pulumi.Provider.PropertyValue) =
        value.Match(
            (fun _ -> raise "null"),
            (fun _ -> raise "bool"),
            (fun num -> 
                JsonValue.Create(num) 
                :> JsonNode
                |> Some
            ),
            (fun _ -> raise "string"),
            (fun _ -> raise "array"),
            (fun _ -> raise "object"),
            (fun _ -> raise "asset"),
            (fun _ -> raise "archive"),
            (fun secret -> writer secret),
            (fun _ -> raise "resource"),            
            (fun output -> writer output.Value),
            (fun _ -> raise "computed")
        )

    let reader (value : JsonElement) = 
        if value.ValueKind = JsonValueKind.Number then
            Pulumi.Provider.PropertyValue(value.GetDouble())
        else 
            failwithf "Invalid JSON document expected number got %O" value.ValueKind

    TypeSpec {
        Description = description jsonSchema.Keywords
        Schema = schema
        Writer = wrapWriter jsonSchema writer
        Reader = reader
    }

let convertSimpleUnion (jsonSchema : Json.Schema.JsonSchema) : Conversion =
    let typ = jsonSchema.Keywords |> pickKeyword<Json.Schema.TypeKeyword> |> Option.get
    
    let nullConversion =
        if typ.Type.HasFlag Json.Schema.SchemaValueType.Null then
            Some (convertNullSchema jsonSchema)
        else None

    let numberConversion =
        if typ.Type.HasFlag Json.Schema.SchemaValueType.Number then
            Some (convertNumberSchema jsonSchema)
        else None

    let stringConversion =
        if typ.Type.HasFlag Json.Schema.SchemaValueType.String then
            Some (convertStringSchema jsonSchema)
        else None

    let boolConversion =
        if typ.Type.HasFlag Json.Schema.SchemaValueType.Boolean then
            Some (convertBoolSchema jsonSchema)
        else None

    let oneof = JsonArray()
    [nullConversion; numberConversion; stringConversion; boolConversion]
    |> List.iter (function 
        | None -> ()
        | Some conversion -> oneof.Add(conversion.Schema))

    let expectedTypes =         
        ["null", nullConversion; "number", numberConversion; "string", stringConversion; "bool", boolConversion]
        |> List.choose (fun (k, opt) -> match opt with | Some _ -> Some k | None -> None)
        |> String.concat " or "
        
    let schema = JsonObject()
    schema.Add("oneOf", oneof)

    let raise (typ : string) = failwithf "Invalid type expected %s got %s" expectedTypes typ
    let rec writer (value : Pulumi.Provider.PropertyValue) =
        value.Match(
            (fun _ -> 
                match nullConversion with 
                | None -> raise "null"
                | Some conversion -> conversion.Writer value
            ),
            (fun _ -> 
                match boolConversion with 
                | None -> raise "bool"
                | Some conversion -> conversion.Writer value
            ),
            (fun _ -> 
                match numberConversion with 
                | None -> raise "number"
                | Some conversion -> conversion.Writer value
            ),
            (fun _ -> 
                match stringConversion with 
                | None -> raise "string"
                | Some conversion -> conversion.Writer value
            ),
            (fun _ -> raise "array"),
            (fun _ -> raise "object"),
            (fun _ -> raise "asset"),
            (fun _ -> raise "archive"),
            (fun secret -> writer secret),
            (fun _ -> raise "resource"),            
            (fun output -> writer output.Value),
            (fun _ -> raise "computed")
        )

    let reader (value : JsonElement) = 
        if value.ValueKind = JsonValueKind.Null && nullConversion.IsSome then
            nullConversion.Value.Reader value
        elif (value.ValueKind = JsonValueKind.True || value.ValueKind = JsonValueKind.False) && boolConversion.IsSome then
            boolConversion.Value.Reader value
        elif value.ValueKind = JsonValueKind.Number && numberConversion.IsSome then
            numberConversion.Value.Reader value
        elif value.ValueKind = JsonValueKind.String && stringConversion.IsSome then
            stringConversion.Value.Reader value
        else 
            failwithf "Invalid JSON document expected %s got %O" expectedTypes value.ValueKind

    TypeSpec {
        Description = description jsonSchema.Keywords
        Schema = schema
        Writer = wrapWriter jsonSchema writer
        Reader = reader
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

and convertArraySchema (root : RootInformation) (jsonSchema : Json.Schema.JsonSchema) : Conversion =
    let schema = JsonObject()
    schema.Add("type", JsonValue.Create("array"))

    let itemsKeyword = jsonSchema.Keywords |> pickKeyword<Json.Schema.ItemsKeyword>

    let items =
        itemsKeyword
        |> Option.map (fun ik ->
            convertSubSchema root ik.SingleSchema
        )
        |> Option.defaultValue (Some (Any None))

    match items with 
    | None -> failwith "array with false items not yet supported"
    | Some items -> schema.Add("items", items.Schema)

    let raise (typ : string) = failwithf "Invalid type expected array got %s" typ
    let rec writer (value : Pulumi.Provider.PropertyValue) =
        value.Match(
            (fun _ -> raise "null"),
            (fun _ -> raise "bool"),
            (fun _ -> raise "number"),
            (fun _ -> raise "string"),
            (fun obj -> 
                obj
                |> Seq.map (fun item ->
                    match items with 
                    | Some conversion -> 
                        conversion.Writer item
                        |> Option.toObj
                    | None ->
                        failwithf "Could not write item")
                |> Seq.toArray
                |> JsonArray
                :> JsonNode
                |> Some
            ),
            (fun _ -> raise "object"),
            (fun _ -> raise "asset"),
            (fun _ -> raise "archive"),
            (fun secret -> writer secret),
            (fun _ -> raise "resource"),            
            (fun output -> writer output.Value),
            (fun _ -> raise "computed")
        )

    let reader (value : JsonElement) = 
        if value.ValueKind = JsonValueKind.Array then
            value.EnumerateArray()
            |> Seq.map (fun item ->
                match items with 
                | Some conversion -> conversion.Reader item
                | None ->
                    failwith "Could not read item")
            |> ImmutableArray.CreateRange
            |> Pulumi.Provider.PropertyValue
        else 
            failwithf "Invalid JSON document expected array got %O" value.ValueKind

    TypeSpec {
        Description = description jsonSchema.Keywords
        Schema = schema
        Writer = wrapWriter jsonSchema writer
        Reader = reader
    }

and convertObjectSchema (root : RootInformation) (jsonSchema : Json.Schema.JsonSchema) : Conversion =
    let schema = JsonObject()
    schema.Add("type", JsonValue.Create("object"))

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
        | None -> Some (Any None)

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
        | Some apk, None ->
            schema.Add("additionalProperties", apk.Schema)
            properties 
        | _, properties -> properties

    let required = 
        match requiredKeyword with
        | None -> Seq.empty
        | Some required -> required.Properties
        |> Set.ofSeq 

    if required.Count <> 0 then
        let requiredArray = 
            required
            |> Seq.map (fun str -> JsonValue.Create(str) :> JsonNode)
            |> Seq.toArray
            |> JsonArray
        schema.Add("required", requiredArray)
    
    match properties with 
    | None -> ()
    | Some properties ->
        let propertiesSchema = JsonObject()
        for kv in properties do
            propertiesSchema.Add(kv.Key, kv.Value.Schema)
            kv.Value.Description
            |> Option.iter(fun desc -> kv.Value.Schema.Add("description", desc))
        schema.Add("properties", propertiesSchema)

    let raise (typ : string) = failwithf "Invalid type expected object got %s" typ
    let rec writer (value : Pulumi.Provider.PropertyValue) =
        value.Match(
            (fun _ -> raise "null"),
            (fun _ -> raise "bool"),
            (fun _ -> raise "number"),
            (fun _ -> raise "string"),
            (fun _ -> raise "array"),
            (fun obj ->
                // Check all the required keys are present
                for key in required do 
                    if not (obj.ContainsKey key) then
                        failwithf "property '%s' is required" key

                obj
                |> Seq.collect (fun kv ->
                    match properties, additionalProperties with
                    | None, None -> 
                        // No properties or additionalProperties
                        failwithf "unexpected property '%s'" kv.Key
                    | Some properties, None -> 
                        match properties.TryFind kv.Key with 
                        | Some conversion ->
                            let node = conversion.Writer kv.Value
                            match node with 
                            | Some node -> [KeyValuePair.Create(kv.Key, node)] :> seq<_>
                            | None -> [KeyValuePair.Create(kv.Key, null)]
                        | None -> failwithf "unexpected property '%s'" kv.Key
                    | None, Some additionalProperties ->
                        let node = additionalProperties.Writer kv.Value
                        match node with 
                        | Some node -> [KeyValuePair.Create(kv.Key, node)]
                        | None -> [KeyValuePair.Create(kv.Key, null)]
                    | Some properties, Some additionalProperties ->
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
                            match properties.TryFind kv.Key with 
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
            (fun secret -> writer secret),
            (fun _ -> raise "resource"),            
            (fun output -> writer output.Value),
            (fun _ -> raise "computed")
        )

    let reader (value : JsonElement) = 
        if value.ValueKind = JsonValueKind.Object then            
            let properties, additionalProperties =
                value.EnumerateObject()
                |> Seq.fold (fun (propsSoFar, addPropsSoFar) kv ->
                    match properties, additionalProperties with
                    | None, None -> 
                        // No properties or additionalProperties
                        failwithf "unexpected property '%s'" kv.Name
                    | Some properties, None -> 
                        match properties.TryFind kv.Name with 
                        | Some conversion ->
                            KeyValuePair.Create(kv.Name, conversion.Reader kv.Value) :: propsSoFar, []
                        | None -> failwithf "unexpected property '%s'" kv.Name
                    | None, Some additionalProperties ->
                        KeyValuePair.Create(kv.Name, additionalProperties.Reader kv.Value) :: propsSoFar, []
                    | Some properties, Some additionalProperties ->
                        match properties.TryFind kv.Name with 
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

    if properties.IsSome then 
        ComplexTypeSpec {
            Description = description jsonSchema.Keywords
            Schema = schema
            Writer = wrapWriter jsonSchema writer
            Reader = reader
        }
    else
        TypeSpec {
            Description = description jsonSchema.Keywords
            Schema = schema
            Writer = wrapWriter jsonSchema writer
            Reader = reader
        }
        
and convertAllOf (schema : Json.Schema.JsonSchema) (allof : Json.Schema.AllOfKeyword) : Conversion =
    Any (Some "default any for allOf")

and convertOneOf (schema : Json.Schema.JsonSchema) (oneOf : Json.Schema.OneOfKeyword) : Conversion =
    Any (Some "default any for oneOf")   

and convertSubSchema (root : RootInformation) (schema : Json.Schema.JsonSchema) : Conversion option =
    match schema.BoolValue |> Option.ofNullable with
    | Some false -> None
    | Some true -> Some (Any None)
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
            Some (convertNullSchema schema)
        elif isSimpleType Json.Schema.SchemaValueType.Boolean keywords then 
            Some (convertBoolSchema schema)
        elif isSimpleType Json.Schema.SchemaValueType.String keywords then 
            Some (convertStringSchema schema)
        elif isSimpleType Json.Schema.SchemaValueType.Number keywords then 
            Some (convertNumberSchema schema)
        elif isSimpleType Json.Schema.SchemaValueType.Object keywords then 
            Some (convertObjectSchema root schema)
        elif isSimpleType Json.Schema.SchemaValueType.Array keywords then 
            Some (convertArraySchema root schema)
        elif isSimpleUnion keywords then
            Some (convertSimpleUnion schema)
        else 
            let msg =
                keywords
                |> Seq.map (fun kw -> kw.ToString())
                |> String.concat ", "
                |> sprintf "unhandled schema: %s"
            Some (Any (Some msg))

type RootConversion = {
    Schema: JsonObject
    Writer : Pulumi.Provider.PropertyValue -> JsonNode option
    Reader : JsonElement ->  Pulumi.Provider.PropertyValue
}

// Generate a full pulumi schema using the conversion as the function to generate
let convertSchema (uri : Uri) (jsonSchema : JsonElement) : RootConversion =
    let schema = JsonObject()
    schema.Add("name", JsonValue.Create("jsonschema"))
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
    functions.Add("jsonschema:index:read", readFunction)
    functions.Add("jsonschema:index:write", writeFunction)

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
    
    // if the schema is a valid complex type then embed it into types and return that, else we'll embed it directly
    let readObjectType = 
        if conversion.IsComplexType then
            let types = JsonObject()
            schema.Add("types", types)
            let complexTypeSpec = conversion.Schema.Deserialize<JsonObject>()
            conversion.Description
            |> Option.iter (fun desc -> complexTypeSpec.Add("description", desc))
            types.Add("jsonschema:index:root", complexTypeSpec)

            let propertySpec = JsonObject()
            propertySpec.Add("$ref", "#/types/jsonschema:index:root")
            propertySpec
        else
            let propertySpec = conversion.Schema.Deserialize<JsonObject>()
            conversion.Description
            |> Option.iter (fun desc -> propertySpec.Add("description", desc))
            propertySpec
    // Use Deserialize to clone
    let writeObjectType = readObjectType.Deserialize<JsonNode>()

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