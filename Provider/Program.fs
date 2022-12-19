// Given a JSON schema generate a Pulumi schema, and a way to read/write from that pulumi type to the expected json.


module Provider

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Text.Json
open System.Text.Json.Nodes

type Conversion = {
    IsComplex : bool
    Schema : JsonObject
    Writer : Pulumi.Provider.PropertyValue -> JsonNode option
    Reader : JsonElement ->  Pulumi.Provider.PropertyValue
}

type RootInformation = {
    Uri : Uri
    Document : JsonElement
}

// Creates a new Conversion that for the pulumi Any type
let createAnyConversion () = 
    let schema = JsonObject()
    schema.Add("$ref", JsonValue.Create("pulumi.json#/Any"))

    let rec writer (value : Pulumi.Provider.PropertyValue) : JsonNode option =
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

    let reader  (value : JsonElement) = 
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
        else 
            failwith "not implemented"


    {
        IsComplex = false
        Schema = schema
        Writer = writer
        Reader = reader
    }

let pickKeyword<'T when 'T :> Json.Schema.IJsonSchemaKeyword> (keywords : IReadOnlyCollection<Json.Schema.IJsonSchemaKeyword>) : 'T option =
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

let isSimpleType (schemaValueType : Json.Schema.SchemaValueType) (schema : Json.Schema.JsonSchema) : bool =
    schema.Keywords
    |> pickKeyword<Json.Schema.TypeKeyword>
    |> Option.map (fun typ ->
        typ.Type = schemaValueType
    ) |> Option.defaultValue false

let isSimpleUnion (schema : Json.Schema.JsonSchema) : bool =
    schema.Keywords
    |> pickKeyword<Json.Schema.TypeKeyword>
    |> Option.map (fun typ ->
        not (typ.Type.HasFlag Json.Schema.SchemaValueType.Object) &&
        not (typ.Type.HasFlag Json.Schema.SchemaValueType.Array)
    ) |> Option.defaultValue false
        
let convertNullSchema (keywords : IReadOnlyCollection<Json.Schema.IJsonSchemaKeyword>) : Conversion =
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

    {
        IsComplex = false
        Schema = schema
        Writer = writer
        Reader = reader
    }

let convertBoolSchema (keywords : IReadOnlyCollection<Json.Schema.IJsonSchemaKeyword>) : Conversion =
    let schema = JsonObject()
    schema.Add("type", JsonValue.Create("bool"))

    let raise  (typ : string) = failwithf "Invalid type expected bool got %s" typ
    let rec writer (value : Pulumi.Provider.PropertyValue) =
        value.Match(
            (fun _ -> raise "null"),
            (fun str -> 
                JsonValue.Create(str) 
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

    {
        IsComplex = false
        Schema = schema
        Writer = writer
        Reader = reader
    }

let convertStringSchema (keywords : IReadOnlyCollection<Json.Schema.IJsonSchemaKeyword>) : Conversion =
    let schema = JsonObject()
    schema.Add("type", JsonValue.Create("string"))

    let raise (typ : string) = failwithf "Invalid type expected string got %s" typ
    let rec writer (value : Pulumi.Provider.PropertyValue) =
        value.Match(
            (fun _ -> raise "null"),
            (fun _ -> raise "bool"),
            (fun _ -> raise "number"),
            (fun str -> 
                JsonValue.Create(str) 
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

    {
        IsComplex = false
        Schema = schema
        Writer = writer
        Reader = reader
    }

let convertNumberSchema (keywords : IReadOnlyCollection<Json.Schema.IJsonSchemaKeyword>) : Conversion =
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

    {
        IsComplex = false
        Schema = schema
        Writer = writer
        Reader = reader
    }

let convertSimpleUnion (keywords : IReadOnlyCollection<Json.Schema.IJsonSchemaKeyword>) : Conversion =
    let typ = keywords |> pickKeyword<Json.Schema.TypeKeyword> |> Option.get
    
    let nullConversion =
        if typ.Type.HasFlag Json.Schema.SchemaValueType.Null then
            Some (convertNullSchema keywords)
        else None

    let numberConversion =
        if typ.Type.HasFlag Json.Schema.SchemaValueType.Number then
            Some (convertNumberSchema keywords)
        else None

    let stringConversion =
        if typ.Type.HasFlag Json.Schema.SchemaValueType.String then
            Some (convertStringSchema keywords)
        else None

    let boolConversion =
        if typ.Type.HasFlag Json.Schema.SchemaValueType.Boolean then
            Some (convertBoolSchema keywords)
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

    {
        IsComplex = false
        Schema = schema
        Writer = writer
        Reader = reader
    }

    
let rec convertRef (root : RootInformation) (schema : Json.Schema.JsonSchema) (ref : Json.Schema.RefKeyword) : Conversion =
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


and convertArraySchema (root : RootInformation) (keywords : IReadOnlyCollection<Json.Schema.IJsonSchemaKeyword>) : Conversion =
    let schema = JsonObject()
    schema.Add("type", JsonValue.Create("array"))

    let itemsKeyword = keywords |> pickKeyword<Json.Schema.ItemsKeyword>

    let items =
        itemsKeyword
        |> Option.map (fun ik ->
            convertSubSchema root ik.SingleSchema
        )

    match items with 
    | None -> ()
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

    {
        IsComplex = false
        Schema = schema
        Writer = writer
        Reader = reader
    }

and convertObjectSchema (root : RootInformation) (keywords : IReadOnlyCollection<Json.Schema.IJsonSchemaKeyword>) : Conversion =
    let schema = JsonObject()
    schema.Add("type", JsonValue.Create("object"))

    let propertiesKeyword = keywords |> pickKeyword<Json.Schema.PropertiesKeyword>
    let additionalPropertiesKeyword = keywords |> pickKeyword<Json.Schema.AdditionalPropertiesKeyword>
    let requiredKeyword = keywords |> pickKeyword<Json.Schema.RequiredKeyword>

    let properties = 
        propertiesKeyword
        |> Option.map (fun pk ->
            pk.Properties
            |> Seq.map (fun kv ->
                let subConversion = convertSubSchema root kv.Value
                (kv.Key, subConversion)
            )
            |> Map.ofSeq
        )

    let additionalProperties =
        additionalPropertiesKeyword
        |> Option.map (fun apk ->
            convertSubSchema root apk.Schema
        )
        |> Option.orElseWith (fun () ->
            // If we don't have properties or additionalProperties then JSON schema says this is an object of "anything", but pulumi schema of just "type": "object" defaults to string values.
            if properties.IsSome then None
            else 
                Some (createAnyConversion())
        )
     

    match additionalProperties with
    | None -> ()
    | Some additionalProperties -> schema.Add("additionalProperties", additionalProperties.Schema)

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
                |> Seq.map (fun kv ->
                    let maybeConversion = 
                        match properties with
                        | Some properties -> properties.TryFind kv.Key
                        | None -> None
                        |> Option.orElse additionalProperties

                    match maybeConversion with 
                    | Some conversion ->
                        let node = conversion.Writer kv.Value
                        match node with 
                        | Some node -> KeyValuePair.Create(kv.Key, node)
                        | None -> KeyValuePair.Create(kv.Key, null)
                    | None ->
                        failwithf "Could not write property %s" kv.Key)
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
            value.EnumerateObject()
            |> Seq.map (fun kv ->
                let maybeConversion = 
                    match properties with
                    | Some properties -> properties.TryFind kv.Name
                    | None -> None
                    |> Option.orElse additionalProperties

                match maybeConversion with 
                | Some conversion -> 
                    let value = conversion.Reader kv.Value
                    KeyValuePair.Create(kv.Name, value)
                | None ->
                    failwithf "Could not read property %s" kv.Name)
            |> ImmutableDictionary.CreateRange
            |> Pulumi.Provider.PropertyValue
        else 
            failwithf "Invalid JSON document expected object got %O" value.ValueKind

    {
        IsComplex = properties.IsSome
        Schema = schema
        Writer = writer
        Reader = reader
    }

and convertOneOf (schema : Json.Schema.JsonSchema) (oneOf : Json.Schema.OneOfKeyword) : Conversion =
    createAnyConversion()

and convertSubSchema (root : RootInformation) (schema : Json.Schema.JsonSchema) : Conversion =
    match schema.BoolValue |> Option.ofNullable with 
    | Some bool -> 
        failwithf "bool schemas are not implemented"
    | None ->
        let oneOf = pickKeyword<Json.Schema.OneOfKeyword> schema.Keywords
        let ref = pickKeyword<Json.Schema.RefKeyword> schema.Keywords
        
        if ref.IsSome then
            convertRef root schema ref.Value
        elif oneOf.IsSome then
            convertOneOf schema oneOf.Value
        elif isSimpleType Json.Schema.SchemaValueType.Null schema then 
            convertNullSchema schema.Keywords
        elif isSimpleType Json.Schema.SchemaValueType.Boolean schema then 
            convertBoolSchema schema.Keywords
        elif isSimpleType Json.Schema.SchemaValueType.String schema then 
            convertStringSchema schema.Keywords
        elif isSimpleType Json.Schema.SchemaValueType.Number schema then 
            convertNumberSchema schema.Keywords
        elif isSimpleType Json.Schema.SchemaValueType.Object schema then 
            convertObjectSchema root schema.Keywords
        elif isSimpleType Json.Schema.SchemaValueType.Array schema then 
            convertArraySchema root schema.Keywords
        elif isSimpleUnion schema then
            convertSimpleUnion schema.Keywords
        else 
            let msg =
                schema.Keywords
                |> Seq.map (fun kw -> kw.ToString())
                |> String.concat ", "
                |> sprintf "unhandled schema: %s"
            let conversion = createAnyConversion()
            conversion.Schema["description"] <- msg
            conversion

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
    
    // if the schema is a valid complex type then embed it into types and return that, else we'll embed it directly
    let readObjectType = 
        if conversion.IsComplex then
            let types = JsonObject()
            schema.Add("types", types)
            types.Add("root", conversion.Schema)

            let ref = JsonObject()
            ref.Add("$ref", "#/types/root")
            ref
        else 
            conversion.Schema
    // Use Deserialize to clone
    let writeObjectType = readObjectType.Deserialize<JsonNode>()

    let jsonType() =
        let node = JsonObject()
        node.Add("type", JsonValue.Create("string"))
        node

    let mkkv name node = KeyValuePair.Create<string, JsonNode>(name, node)

    readFunction.Add("description", "Read the given JSON into the object model")
    readFunction.Add("inputs", JsonObject([
        mkkv "required" (JsonArray(JsonValue.Create "json"))
        mkkv "properties" (JsonObject([
             mkkv "json" (jsonType())
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
             mkkv "json" (jsonType())
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