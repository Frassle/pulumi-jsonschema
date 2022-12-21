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

let conversionToJson (conversion : Provider.RootConversion) : string =
    conversion.Schema
    :> Nodes.JsonNode
    |> Some
    |> toJson

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
                "properties": {"value": {"$ref":"#/types/schema:index:root"}}
            }
        },
        "schema:index:write":{
            "description":"Read the given JSON into the object model",
            "inputs":{
                "required": ["value"],
                "properties": {"value": {"$ref":"#/types/schema:index:root"}}
            },
            "outputs":{
                "required": ["json"],
                "properties": {"json": {"type": "string"}}
            }
        }
    }
}""" typesJson
    
let dictToProperty (list : (string * Pulumi.Provider.PropertyValue) list) =
    list
    |> Seq.map (fun (k, v) -> KeyValuePair.Create(k, v))
    |> ImmutableDictionary.CreateRange
    |> Pulumi.Provider.PropertyValue

let listToProperty (list : Pulumi.Provider.PropertyValue list) =
    list |> ImmutableArray.CreateRange |> Pulumi.Provider.PropertyValue

let roundTrip (schema : System.Text.Json.JsonElement) (conversion : Provider.RootConversion) =
    // Use Json.Schema.Data to generate some json, check we can read and write it
    let jsonSchema = Json.Schema.JsonSchema.FromText (schema.GetRawText())
    let data = Json.Schema.DataGeneration.JsonSchemaExtensions.GenerateData(jsonSchema)
    if not data.IsSuccess then
        failwithf "Could not generate JSON data: %s" data.ErrorMessage

    let element = data.Result.Deserialize<System.Text.Json.JsonElement>()
    
    let dom = conversion.Reader element
    let rt = conversion.Writer dom
    
    match rt with 
    | None -> "null"
    | Some rt -> rt.ToJsonString()
    |> shouldJsonEqual (element.GetRawText())
