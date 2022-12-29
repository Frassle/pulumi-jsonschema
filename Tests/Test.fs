module Test

open System
open System.Text.Json
open Xunit
open System.Collections.Generic
open System.Collections.Immutable

type JsonComparer() =
    interface IEqualityComparer<JsonElement> with
        member this.Equals(x: JsonElement, y: JsonElement): bool =
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
                    |> Seq.forall (fun (x, y) -> 
                        this.Equals(x, y)
                    )
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
                        |> Option.defaultValue false
                    )
            else 
                false

        member this.GetHashCode(obj: JsonElement): int = 
            obj.GetHashCode()
            
    member this.Equals(x: JsonElement, y: JsonElement): bool =
        (this :> IEqualityComparer<JsonElement>).Equals(x, y)

let shouldEqual<'T> (expected : 'T) (actual : 'T) : unit = 
    Assert.Equal(expected, actual)
    
let fromJson (text : string) : JsonElement = 
    let jsonData = System.Text.Encoding.UTF8.GetBytes text
    let mutable reader = Utf8JsonReader(jsonData)
    JsonElement.ParseValue(&reader)

let toJson (node : Nodes.JsonNode option) : string =
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

let shouldJsonEqual (expected : string) (actual : string) : unit = 
    let expectedJson = fromJson expected
    let actualJson = fromJson actual
    Assert.Equal(expectedJson, actualJson, JsonComparer())

let baseUri = Uri("https://github.com/Frassle/pulumi-jsonschema/schema.json")

// Fills in the standard fields for the Pulumi schema
let simpleSchema (objectType : string) : string =
    sprintf """{
    "name":"schema",
    "description":"A pulumi package generated from a json schema",
    "keywords":["pulumi","jsonschema"],
    "homepage":"https://github.com/Frassle/pulumi-jsonschema",
    "repository":"https://github.com/Frassle/pulumi-jsonschema",
    "license":"Apache-2.0",
    "functions":{
        "schema:index:read":{
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
        "schema:index:write":{
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
}""" objectType objectType

let complexSchema (types : (string*string) list) : string =
    let typesJson = 
        types 
        |> Seq.map (fun (k, v) -> sprintf "\"%s\": %s" k v)
        |> String.concat ","

    let rootType = 
        types
        |> Seq.head
        |> fst

    sprintf """{
    "name":"schema",
    "description":"A pulumi package generated from a json schema",
    "keywords":["pulumi","jsonschema"],
    "homepage":"https://github.com/Frassle/pulumi-jsonschema",
    "repository":"https://github.com/Frassle/pulumi-jsonschema",
    "license":"Apache-2.0",
    "types": {%s},
    "functions":{
        "schema:index:read":{
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
        "schema:index:write":{
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
}""" typesJson rootType rootType
    
let dictToProperty (list : (string * Pulumi.Provider.PropertyValue) list) =
    list
    |> Seq.map (fun (k, v) -> KeyValuePair.Create(k, v))
    |> ImmutableDictionary.CreateRange
    |> Pulumi.Provider.PropertyValue

let listToProperty (list : Pulumi.Provider.PropertyValue list) =
    list |> ImmutableArray.CreateRange |> Pulumi.Provider.PropertyValue

type SchemaTest = {
    Schema : Json.Schema.JsonSchema
    Conversion : JsonSchema.Provider.RootConversion
} with 

    member this.ShouldWrite (expectedJson : string) (value : Pulumi.Provider.PropertyValue) : unit =
        let result = this.Conversion.Writer value

        result
        |> toJson
        |> shouldJsonEqual expectedJson
    
        let validationOptions = Json.Schema.ValidationOptions()
        validationOptions.OutputFormat <- Json.Schema.OutputFormat.Basic
        let validation = this.Schema.Validate(Option.toObj result, validationOptions)
        if not validation.IsValid then 
            failwith validation.Message

    member this.ShouldRead (expectedValue : Pulumi.Provider.PropertyValue) (json : string) : unit =
        let validationOptions = Json.Schema.ValidationOptions()
        validationOptions.OutputFormat <- Json.Schema.OutputFormat.Basic
    
        let element = fromJson json
        let node = Json.More.JsonElementExtensions.AsNode(element)
        let validation = this.Schema.Validate(node, validationOptions)
        if not validation.IsValid then 
            failwith validation.Message

        this.Conversion.Reader element 
        |> shouldEqual expectedValue
        
    member this.ShouldThrow<'T when 'T :> exn>(value : Pulumi.Provider.PropertyValue) : 'T =
        Assert.Throws<'T>(fun () ->
            this.Conversion.Writer value
            |> ignore)
        
    member this.ShouldThrow<'T when 'T :> exn>(json : string) : 'T =
        let validationOptions = Json.Schema.ValidationOptions()
        validationOptions.OutputFormat <- Json.Schema.OutputFormat.Basic
    
        let element = fromJson json
        let node = Json.More.JsonElementExtensions.AsNode(element)
        let validation = this.Schema.Validate(node, validationOptions)
        if validation.IsValid then 
            failwith validation.Message

        Assert.Throws<'T>(fun () ->
            fromJson json
            |> this.Conversion.Reader 
            |> ignore)

    member this.ShouldRoundTrip (json : string) (value : Pulumi.Provider.PropertyValue) : unit =
        value 
        |> this.ShouldWrite json
        json
        |> this.ShouldRead value

    member this.RoundTrip () =
        // Use Json.Schema.Data to generate some json, check we can read and write it
        let data = Json.Schema.DataGeneration.JsonSchemaExtensions.GenerateData(this.Schema)
        if not data.IsSuccess then
            failwithf "Could not generate JSON data: %s" data.ErrorMessage

        let element = data.Result.Deserialize<System.Text.Json.JsonElement>()
    
        let dom = this.Conversion.Reader element
        let rt = this.Conversion.Writer dom
    
        match rt with 
        | None -> "null"
        | Some rt -> rt.ToJsonString()
        |> shouldJsonEqual (element.GetRawText())

    member this.ShouldEqual (pulumiSchema : string) =    
        this.Conversion.Schema
        :> Nodes.JsonNode
        |> Some
        |> toJson
        |> shouldJsonEqual pulumiSchema

let convertSchema (schema: string) =
    let json = System.Text.Json.JsonDocument.Parse schema
    let schema = Json.Schema.JsonSchema.FromText schema
    {
        Schema = schema
        Conversion = JsonSchema.Provider.convertSchema baseUri json.RootElement
    }
