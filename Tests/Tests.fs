module Tests

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

let testBaseUri = Uri("https://github.com/Frassle/pulumi-jsonschema/schema.json")

// Fills in the standard fields for the Pulumi schema
let simpleSchema (objectType : string) : string =
    sprintf """{
    "name":"jsonschema",
    "description":"A pulumi package generated from a json schema",
    "keywords":["pulumi","jsonschema"],
    "homepage":"https://github.com/Frassle/pulumi-jsonschema",
    "repository":"https://github.com/Frassle/pulumi-jsonschema",
    "license":"Apache-2.0",
    "functions":{
        "jsonschema:index:read":{
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
        "jsonschema:index:write":{
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
    "name":"jsonschema",
    "description":"A pulumi package generated from a json schema",
    "keywords":["pulumi","jsonschema"],
    "homepage":"https://github.com/Frassle/pulumi-jsonschema",
    "repository":"https://github.com/Frassle/pulumi-jsonschema",
    "license":"Apache-2.0",
    "types": {%s},
    "functions":{
        "jsonschema:index:read":{
            "description":"Read the given JSON into the object model",
            "inputs":{
                "required": ["json"],
                "properties": {"json": {"type": "string"}}
            },
            "outputs":{
                "required": ["value"],
                "properties": {"value": {"$ref":"#/types/root"}}
            }
        },
        "jsonschema:index:write":{
            "description":"Read the given JSON into the object model",
            "inputs":{
                "required": ["value"],
                "properties": {"value": {"$ref":"#/types/root"}}
            },
            "outputs":{
                "required": ["json"],
                "properties": {"json": {"type": "string"}}
            }
        }
    }
}""" typesJson
    
let dictToMap<'K, 'V when 'K: comparison> (dict : ImmutableDictionary<'K, 'V>) : Map<'K, 'V> =
    dict 
    |> Seq.map (fun kv -> kv.Key, kv.Value)
    |> Map.ofSeq

[<Fact>]
let ``Test null`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "null" 
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement

    // Pulumi schema doesn't support null, so we say it's an anything but only allow null as a value
    conversion
    |> conversionToJson
    |> shouldJsonEqual (simpleSchema """{"$ref":"pulumi.json#/Any"}""")

    Pulumi.Provider.PropertyValue.Null
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual "null"

    fromJson "null"
    |> conversion.Reader
    |> fun value -> value.Match<unit>(
        (fun () -> ()),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"))

[<Fact>]
let ``Test bool`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "boolean" 
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (simpleSchema """{"type":"bool"}""")

    Pulumi.Provider.PropertyValue(true)
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual "true"

    fromJson "true"
    |> conversion.Reader
    |> fun value -> value.Match<unit>(
        (fun _ -> failwith "unexpected"),
        (shouldEqual true),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"))

[<Fact>]
let ``Test string`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "string" 
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (simpleSchema """{"type":"string"}""")

    Pulumi.Provider.PropertyValue("test")
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual "\"test\""

    fromJson "\"test\""
    |> conversion.Reader
    |> fun value -> value.Match<unit>(
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (shouldEqual "test"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"))

[<Fact>]
let ``Test number`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "number" 
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (simpleSchema """{"type":"number"}""")

    Pulumi.Provider.PropertyValue(14.512)
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual "14.512"

    fromJson "53.42"
    |> conversion.Reader
    |> fun value -> value.Match<unit>(
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (shouldEqual 53.42),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"))
        
[<Fact>]
let ``Test array`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "array",
        "items": { "type": "string" }
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (simpleSchema """{"type":"array","items":{"type":"string"}}""")
    
    Pulumi.Provider.PropertyValue(ImmutableArray.CreateRange [
        Pulumi.Provider.PropertyValue("a");
        Pulumi.Provider.PropertyValue("b");
    ])
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """["a","b"]"""

    fromJson """["foo","bar"]"""
    |> conversion.Reader
    |> fun value -> value.Match<unit>(
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun arr -> 
            arr
            |> Seq.toList
            |> shouldEqual ([Pulumi.Provider.PropertyValue("foo"); Pulumi.Provider.PropertyValue("bar")])),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"))

[<Fact>]
let ``Test empty object`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object" 
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (simpleSchema """{
        "type":"object",
        "additionalProperties": {
            "$ref": "pulumi.json#/Any"
        }
    }""")
    
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual "{}"
    
    Pulumi.Provider.PropertyValue(ImmutableDictionary.CreateRange([
        KeyValuePair.Create("hello", Pulumi.Provider.PropertyValue("a"));
        KeyValuePair.Create("test", Pulumi.Provider.PropertyValue(123));
    ]))
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """{"hello": "a", "test": 123}"""

    fromJson "{}"
    |> conversion.Reader
    |> fun value -> value.Match<unit>(
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun obj -> obj.Count |> shouldEqual 0),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"))

[<Fact>]
let ``Test object with additional properties`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object",
        "additionalProperties": { "type": "string" }
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (simpleSchema """{"type":"object","additionalProperties":{"type":"string"}}""")
    
    Pulumi.Provider.PropertyValue(ImmutableDictionary.CreateRange [
        KeyValuePair.Create("hello", Pulumi.Provider.PropertyValue("a"));
        KeyValuePair.Create("test", Pulumi.Provider.PropertyValue("b"));
    ])
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """{"test":"b","hello":"a"}"""

    fromJson """{"a":"string","b":"number"}"""
    |> conversion.Reader
    |> fun value -> value.Match<unit>(
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun obj -> 
            obj 
            |> dictToMap
            |> shouldEqual (Map.ofList ["a", Pulumi.Provider.PropertyValue("string"); "b", Pulumi.Provider.PropertyValue("number")])),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"))

[<Fact>]
let ``Test object with properties`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object",
        "properties": {
            "foo": { "type": "string" }
        },
        "additionalProperties": false
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (complexSchema ["root", """{"type":"object","properties":{"foo":{"type":"string"}}}"""])
    
    Pulumi.Provider.PropertyValue(ImmutableDictionary.CreateRange [
        KeyValuePair.Create("foo", Pulumi.Provider.PropertyValue("a"));
    ])
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """{"foo":"a"}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """{}"""

    fromJson """{"foo":"string"}"""
    |> conversion.Reader
    |> fun value -> value.Match<unit>(
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun obj -> 
            obj 
            |> dictToMap
            |> shouldEqual (Map.ofList ["foo", Pulumi.Provider.PropertyValue("string")])),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"))

    fromJson """{}"""
    |> conversion.Reader
    |> fun value -> value.Match<unit>(
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun obj -> obj.Count |> shouldEqual 0),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"))

[<Fact>]
let ``Test object with required properties`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object",
        "properties": {
            "foo": { "type": "string" }
        },
        "additionalProperties": false,
        "required": ["foo"]
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (complexSchema ["root", """{
        "type":"object",
        "properties":{"foo":{"type":"string"}},
        "required": ["foo"]
    }"""])
    
    Pulumi.Provider.PropertyValue(ImmutableDictionary.CreateRange [
        KeyValuePair.Create("foo", Pulumi.Provider.PropertyValue("a"));
    ])
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """{"foo":"a"}"""

    
    let exc = Assert.Throws<Exception>(fun () ->
        Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
        |> conversion.Writer
        |> ignore
    )
    exc.Message |> shouldEqual "property 'foo' is required"

    fromJson """{"foo":"string"}"""
    |> conversion.Reader
    |> fun value -> value.Match<unit>(
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun obj -> 
            obj 
            |> dictToMap
            |> shouldEqual (Map.ofList ["foo", Pulumi.Provider.PropertyValue("string")])),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"))

[<Fact>]
let ``Test refs`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object",
        "additionalProperties": false,
        "properties": {
            "foo": { "$ref": "#/$defs/myType" }
        },
        "$defs": {
            "myType": { "type": "number" }
        }
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (complexSchema ["root", """{"type":"object","properties":{"foo":{"type":"number"}}}"""])
    
    Pulumi.Provider.PropertyValue(ImmutableDictionary.CreateRange [
        KeyValuePair.Create("foo", Pulumi.Provider.PropertyValue(123));
    ])
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """{"foo":123}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """{}"""

    fromJson """{"foo":456.789}"""
    |> conversion.Reader
    |> fun value -> value.Match<unit>(
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun obj -> 
            obj 
            |> dictToMap
            |> shouldEqual (Map.ofList ["foo", Pulumi.Provider.PropertyValue(456.789)])),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"))

    fromJson """{}"""
    |> conversion.Reader
    |> fun value -> value.Match<unit>(
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun obj -> obj.Count |> shouldEqual 0),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"))

[<Fact>]
let ``Test simple type union`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": ["boolean", "string"]
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (simpleSchema """{"oneOf": [
            {"type": "string"},
            {"type": "bool"}
        ]
    }""")

    Pulumi.Provider.PropertyValue(true)
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual "true"

    Pulumi.Provider.PropertyValue("hello")
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual "\"hello\""

    fromJson "true"
    |> conversion.Reader
    |> fun value -> value.Match<unit>(
        (fun _ -> failwith "unexpected"),
        (shouldEqual true),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"))

    fromJson "\"testing\""
    |> conversion.Reader
    |> fun value -> value.Match<unit>(
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (shouldEqual "testing"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"),
        (fun _ -> failwith "unexpected"))
        
[<Fact>]
let ``Test githhub`` () =
    let uri = Uri("https://raw.githubusercontent.com/SchemaStore/schemastore/master/src/schemas/json/github-workflow.json")
    let schema = 
        use client = new System.Net.Http.HttpClient()
        let contents = client.GetStringAsync(uri)
        System.Text.Json.JsonDocument.Parse contents.Result
        
    let conversion = Provider.convertSchema uri schema.RootElement

    Assert.NotNull(conversion)

[<Fact>]
let ``Test pulumi`` () =
    let uri = Uri("https://raw.githubusercontent.com/pulumi/pulumi/569369d6f18120d0074d07eac656745f6f7244bf/pkg/codegen/schema/pulumi.json")
    let schema = 
        use client = new System.Net.Http.HttpClient()
        let contents = client.GetStringAsync(uri)
        System.Text.Json.JsonDocument.Parse contents.Result
        
    let conversion = Provider.convertSchema uri schema.RootElement

    Assert.NotNull(conversion)

    // What's "fun" is that this is the schema for pulumi schema,
    // and so we've generated a pulumi schema for pulumi schema,
    // which means it should be able to read itself
    let pulumiSchemaDocument = conversion.Schema.Deserialize<JsonElement>()
    let dom = conversion.Reader pulumiSchemaDocument
    Assert.NotNull(dom)