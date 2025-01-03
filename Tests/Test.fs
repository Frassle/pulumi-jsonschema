module Test

open System
open System.Text.Json
open Xunit
open System.Collections.Generic
open System.Collections.Immutable
open Pulumi.Experimental.Provider

type JsonComparer() =
    interface IEqualityComparer<JsonElement> with
        member this.Equals(x: JsonElement, y: JsonElement) : bool =
            if x.ValueKind = JsonValueKind.Null && y.ValueKind = JsonValueKind.Null then
                true
            elif x.ValueKind = JsonValueKind.False && y.ValueKind = JsonValueKind.False then
                true
            elif x.ValueKind = JsonValueKind.True && y.ValueKind = JsonValueKind.True then
                true
            elif x.ValueKind = JsonValueKind.String && y.ValueKind = JsonValueKind.String then
                x.GetString() = y.GetString()
            elif x.ValueKind = JsonValueKind.Number && y.ValueKind = JsonValueKind.Number then
                x.GetDouble() = y.GetDouble()
            elif x.ValueKind = JsonValueKind.Array && y.ValueKind = JsonValueKind.Array then
                if x.GetArrayLength() <> y.GetArrayLength() then
                    false
                else
                    x.EnumerateArray()
                    |> Seq.zip (y.EnumerateArray())
                    |> Seq.forall (fun (x, y) -> this.Equals(x, y))
            elif x.ValueKind = JsonValueKind.Object && y.ValueKind = JsonValueKind.Object then
                let x = x.EnumerateObject() |> Seq.map (fun kv -> kv.Name, kv.Value) |> Map.ofSeq
                let y = y.EnumerateObject() |> Seq.map (fun kv -> kv.Name, kv.Value) |> Map.ofSeq

                if x.Count <> y.Count then
                    false
                else
                    x
                    |> Seq.forall (fun kv ->
                        Map.tryFind kv.Key y
                        |> Option.map (fun v -> this.Equals(kv.Value, v))
                        |> Option.defaultValue false)
            else
                false

        member this.GetHashCode(obj: JsonElement) : int = obj.GetHashCode()

    member this.Equals(x: JsonElement, y: JsonElement) : bool =
        (this :> IEqualityComparer<JsonElement>).Equals(x, y)

let shouldEqual<'T> (expected: 'T) (actual: 'T) : unit = Assert.Equal(expected, actual)

let fromJson (text: string) : JsonElement =
    let jsonData = System.Text.Encoding.UTF8.GetBytes text
    let mutable reader = Utf8JsonReader(jsonData)
    JsonElement.ParseValue(&reader)

let toJson (node: Nodes.JsonNode option) : string =
    use stream = new System.IO.MemoryStream()
    let mutable writerOptions = JsonWriterOptions()
    writerOptions.Indented <- true
    use writer = new Utf8JsonWriter(stream, writerOptions)
    let mutable serializerOptions = JsonSerializerOptions()
    serializerOptions.WriteIndented <- true

    match node with
    | Some node -> node.WriteTo(writer, serializerOptions)
    | None -> writer.WriteNullValue()

    writer.Flush()
    System.Text.Encoding.UTF8.GetString(stream.ToArray())

let shouldJsonEqual (expected: string) (actual: string) : unit =
    let expectedJson = fromJson expected
    let actualJson = fromJson actual
    Assert.Equal(expectedJson, actualJson, JsonComparer())

let baseUri = Uri("https://github.com/Frassle/pulumi-jsonschema/schema.json")

let dictToProperty (list: (string * PropertyValue) list) =
    list
    |> Seq.map (fun (k, v) -> KeyValuePair.Create(k, v))
    |> ImmutableDictionary.CreateRange
    |> PropertyValue

let listToProperty (list: PropertyValue list) =
    list |> ImmutableArray.CreateRange |> PropertyValue

type SchemaTest =
    { 
      Schema: Json.Schema.JsonSchema
      Conversion: JsonSchema.Converter.RootConversion }

    member this.ShouldWrite (expectedJson: string) (value: PropertyValue) : unit =
        let result = this.Conversion.Writer value

        result |> toJson |> shouldJsonEqual expectedJson


        let evaluationOptions = Json.Schema.EvaluationOptions()
        evaluationOptions.OutputFormat <- Json.Schema.OutputFormat.Flag
        let evaluation = this.Schema.Evaluate(Option.toObj result, evaluationOptions)

        if not evaluation.IsValid then
            failwithf "%A" evaluation.Errors

    member this.ShouldRead (expectedValue: PropertyValue) (json: string) : unit =
        let evaluationOptions = Json.Schema.EvaluationOptions()
        evaluationOptions.OutputFormat <- Json.Schema.OutputFormat.Flag

        let element = fromJson json
        let node = Json.More.JsonElementExtensions.AsNode(element)
        let evaluation = this.Schema.Evaluate(node, evaluationOptions)

        if not evaluation.IsValid then
            failwithf "%A" evaluation.Errors

        this.Conversion.Reader element |> shouldEqual expectedValue

    member this.ShouldThrow<'T when 'T :> exn>(value: PropertyValue) : 'T =
        Assert.Throws<'T>(fun () -> this.Conversion.Writer value |> ignore)

    member this.ShouldThrow<'T when 'T :> exn>(json: string) : 'T =
        let evaluationOptions = Json.Schema.EvaluationOptions()
        evaluationOptions.OutputFormat <- Json.Schema.OutputFormat.Flag

        let element = fromJson json
        let node = Json.More.JsonElementExtensions.AsNode(element)
        let evaluation = this.Schema.Evaluate(node, evaluationOptions)

        if evaluation.IsValid then
            failwithf "%A" evaluation.Errors

        Assert.Throws<'T>(fun () -> fromJson json |> this.Conversion.Reader |> ignore)

    member this.ShouldRoundTrip (json: string) (value: PropertyValue) : unit =
        value |> this.ShouldWrite json
        json |> this.ShouldRead value

    member this.RoundTrip() =
        // Use Json.Schema.Data to generate some json, check we can read and write it
        let data = lock this.Schema (fun () ->
            Json.Schema.DataGeneration.JsonSchemaExtensions.GenerateData(this.Schema))

        if not data.IsSuccess then
            failwithf "Could not generate JSON data: %s" data.ErrorMessage

        let element = data.Result.Deserialize<System.Text.Json.JsonElement>()

        let dom = this.Conversion.Reader element
        let rt = this.Conversion.Writer dom

        match rt with
        | None -> "null"
        | Some rt -> rt.ToJsonString()
        |> shouldJsonEqual (element.GetRawText())

    member this.ShouldEqual(pulumiSchema: string) =
        this.Conversion.Schema :> Nodes.JsonNode
        |> Some
        |> toJson
        |> shouldJsonEqual pulumiSchema

        
    // Fills in the standard fields for the Pulumi schema
    member this.SimpleSchema (objectType: string) : string =
        let parameterization = this.Conversion.Schema.Item "parameterization"
        let parameter = parameterization.Item "parameter"
        let base64 = parameter.GetValue<string>()

        sprintf
            """{
        "name":"test",
        "version": "1.0.0",
        "description":"A pulumi package generated from a json schema",
        "keywords":["pulumi","jsonschema"],
        "homepage":"https://github.com/Frassle/pulumi-jsonschema",
        "repository":"https://github.com/Frassle/pulumi-jsonschema",
        "license":"Apache-2.0",
        "pluginDownloadURL": "github://api.github.com/Frassle",
        "parameterization": {
            "baseProvider": {
                "version": "0.1.1",
                "name": "jsonschema"
            },
            "parameter": "%s"
        },
        "functions":{
            "test:index:read":{
                "description":"Read the given JSON into the object model",
                "inputs":{
                    "required": ["json"],
                    "properties": {"json": {"type": "string"}}
                },
                "outputs":{
                    "required": ["value"],
                    "properties": {"value": %s}
                }
            },
            "test:index:write":{
                "description":"Read the given JSON into the object model",
                "inputs":{
                    "required": ["value"],
                    "properties": {"value": %s}
                },
                "outputs":{
                    "required": ["json"],
                    "properties": {"json": {"type": "string"}}
                }
            }
        }
    }"""
            base64
            objectType
            objectType

    member this.ComplexSchema (types: (string * string) list) : string =
        let parameterization = this.Conversion.Schema.Item "parameterization"
        let parameter = parameterization.Item "parameter"
        let base64 = parameter.GetValue<string>()

        let typesJson =
            types |> Seq.map (fun (k, v) -> sprintf "\"%s\": %s" k v) |> String.concat ","

        let rootType = types |> Seq.head |> fst

        sprintf
            """{
        "name":"test",
        "version": "1.0.0",
        "description":"A pulumi package generated from a json schema",
        "keywords":["pulumi","jsonschema"],
        "homepage":"https://github.com/Frassle/pulumi-jsonschema",
        "repository":"https://github.com/Frassle/pulumi-jsonschema",
        "license":"Apache-2.0",
        "pluginDownloadURL": "github://api.github.com/Frassle",
        "parameterization": {
            "baseProvider": {
                "version": "0.1.1",
                "name": "jsonschema"
            },
            "parameter": "%s"
        },
        "types": {%s},
        "functions":{
            "test:index:read":{
                "description":"Read the given JSON into the object model",
                "inputs":{
                    "required": ["json"],
                    "properties": {"json": {"type": "string"}}
                },
                "outputs":{
                    "required": ["value"],
                    "properties": {"value": {"$ref":"#/types/%s"}}
                }
            },
            "test:index:write":{
                "description":"Read the given JSON into the object model",
                "inputs":{
                    "required": ["value"],
                    "properties": {"value": {"$ref":"#/types/%s"}}
                },
                "outputs":{
                    "required": ["json"],
                    "properties": {"json": {"type": "string"}}
                }
            }
        }
    }"""
            base64
            typesJson
            rootType
            rootType

let convertSchema (schema: string) =
    let json = System.Text.Json.JsonDocument.Parse schema
    let schema = Json.Schema.JsonSchema.FromText schema

    { 
      Schema = schema
      Conversion = JsonSchema.Converter.convertSchema baseUri "test" json.RootElement }
