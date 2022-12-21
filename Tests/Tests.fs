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
    
let listToDict<'K, 'V> (list : ('K * 'V) list) : ImmutableDictionary<'K, 'V> =
    list
    |> Seq.map (fun (k, v) -> KeyValuePair.Create(k, v))
    |> ImmutableDictionary.CreateRange

let testRoundTrip (schema : System.Text.Json.JsonElement) (conversion : Provider.RootConversion) =
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

[<Fact>]
let ``Test empty`` () =
    let schema = System.Text.Json.JsonDocument.Parse "{}"
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion

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
    |> shouldEqual Pulumi.Provider.PropertyValue.Null


[<Fact>]
let ``Test null`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "null" 
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion

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
    |> shouldEqual Pulumi.Provider.PropertyValue.Null

[<Fact>]
let ``Test bool`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "boolean" 
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (simpleSchema """{"type":"boolean"}""")

    Pulumi.Provider.PropertyValue(true)
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual "true"

    fromJson "true"
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue true)

[<Fact>]
let ``Test string`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "string" 
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (simpleSchema """{"type":"string"}""")

    Pulumi.Provider.PropertyValue("test")
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual "\"test\""

    fromJson "\"test\""
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue "test")

[<Fact>]
let ``Test string enum`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "string",
        "enum": ["info", "warn", "error"]
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (complexSchema [
        "schema:index:root", """{"type":"string","enum":[{"value":"info"},{"value":"warn"},{"value":"error"}]}"""
    ])

    Pulumi.Provider.PropertyValue("info")
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual "\"info\""

    fromJson "\"info\""
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue "info")

    let exc = Assert.Throws<Exception>(fun () ->
        Pulumi.Provider.PropertyValue("badenum")
        |> conversion.Writer
        |> ignore
    )
    exc.Message |> shouldEqual "Expected value to match one of the values specified by the enum"

[<Fact>]
let ``Test number`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "number" 
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (simpleSchema """{"type":"number"}""")

    Pulumi.Provider.PropertyValue(14.512)
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual "14.512"

    fromJson "53.42"
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue 53.42)

[<Fact>]
let ``Test array`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "array",
        "items": { "type": "string" }
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion
    
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
    |> shouldEqual (Pulumi.Provider.PropertyValue (ImmutableArray.CreateRange [
        Pulumi.Provider.PropertyValue("foo")
        Pulumi.Provider.PropertyValue("bar")
    ]))
    
[<Fact>]
let ``Test tuple`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "array",
        "prefixItems": [
            { "type": "string" },
            { "type": "number" }
        ],
        "items": false
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (complexSchema ["schema:index:root", """{
        "type":"object",
        "properties":{
            "item1" : { "type":"string" },
            "item2" : { "type":"number" }
        }
    }"""])
    
    Pulumi.Provider.PropertyValue(listToDict [
        "item1", Pulumi.Provider.PropertyValue("a")
        "item2", Pulumi.Provider.PropertyValue(123)
    ])
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """["a",123]"""

    fromJson """["foo",-345]"""
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue (listToDict [
        "item1", Pulumi.Provider.PropertyValue("foo")
        "item2", Pulumi.Provider.PropertyValue(-345)
    ]))

[<Fact>]
let ``Test empty object`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object" 
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion
    
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
    
    Pulumi.Provider.PropertyValue(listToDict [
        "hello", Pulumi.Provider.PropertyValue("a")
        "test", Pulumi.Provider.PropertyValue(123)
    ])
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """{"hello": "a", "test": 123}"""

    fromJson "{}"
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)

[<Fact>]
let ``Test object with additional properties`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object",
        "additionalProperties": { "type": "string" }
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (simpleSchema """{"type":"object","additionalProperties":{"type":"string"}}""")
    
    Pulumi.Provider.PropertyValue(listToDict [
        "hello", Pulumi.Provider.PropertyValue("a")
        "test", Pulumi.Provider.PropertyValue("b")
    ])
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """{"test":"b","hello":"a"}"""

    fromJson """{"a":"string","b":"number"}"""
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue (listToDict [
        "a", Pulumi.Provider.PropertyValue("string")
        "b", Pulumi.Provider.PropertyValue("number")
    ]))

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
    testRoundTrip schema.RootElement conversion
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (complexSchema ["schema:index:root", """{"type":"object","properties":{"foo":{"type":"string"}}}"""])
    
    Pulumi.Provider.PropertyValue(listToDict [
        "foo", Pulumi.Provider.PropertyValue("a")
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
    |> shouldEqual (Pulumi.Provider.PropertyValue (listToDict [
        "foo", Pulumi.Provider.PropertyValue("string")
    ]))

    fromJson """{}"""
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)

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
    testRoundTrip schema.RootElement conversion
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (complexSchema ["schema:index:root", """{
        "type":"object",
        "properties":{"foo":{"type":"string"}},
        "required": ["foo"]
    }"""])
    
    Pulumi.Provider.PropertyValue(listToDict [
        "foo", Pulumi.Provider.PropertyValue("a")
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
    |> shouldEqual (Pulumi.Provider.PropertyValue (listToDict [
        "foo", Pulumi.Provider.PropertyValue("string")
    ]))
    

[<Fact>]
let ``Test object with properties and additionalProperties`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object",
        "properties": {
            "foo": { "type": "string" }
        },
        "additionalProperties": { "type": "number" }
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (complexSchema ["schema:index:root", """{
        "type":"object",
        "properties":{
            "foo":{"type":"string"},
            "additionalProperties":{"type": "object", "additionalProperties": {"type": "number"}}
        }
    }"""])
    
    Pulumi.Provider.PropertyValue(listToDict [
        "foo", Pulumi.Provider.PropertyValue("a")
        "additionalProperties", Pulumi.Provider.PropertyValue(listToDict [
            "bob", Pulumi.Provider.PropertyValue(123)
        ])
    ])
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """{"foo":"a","bob":123}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """{}"""

    fromJson """{"foo":"string", "other": 54}"""
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue (listToDict [
        "foo", Pulumi.Provider.PropertyValue("string")
        "additionalProperties", Pulumi.Provider.PropertyValue(listToDict [
            "other", Pulumi.Provider.PropertyValue(54)
        ])
    ]))

    fromJson """{}"""
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)

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
    testRoundTrip schema.RootElement conversion
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (complexSchema ["schema:index:root", """{"type":"object","properties":{"foo":{"type":"number"}}}"""])
    
    Pulumi.Provider.PropertyValue(listToDict [
        "foo", Pulumi.Provider.PropertyValue(123)
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
    |> shouldEqual (Pulumi.Provider.PropertyValue (listToDict [
        "foo", Pulumi.Provider.PropertyValue(456.789)
    ]))

    fromJson """{}"""
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)    

[<Fact>]
let ``Test simple type union`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": ["boolean", "string"]
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (simpleSchema """{"oneOf": [
            {"type": "boolean"},
            {"type": "string"}
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
    |> shouldEqual (Pulumi.Provider.PropertyValue true)

    fromJson "\"testing\""
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue "testing")

[<Fact>]
let ``Test simple description`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "number",
        "description": "This is a number"
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (simpleSchema """{
        "type":"number",
        "description": "This is a number"
    }""")

[<Fact>]
let ``Test property description`` () =    
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object",
        "properties": {
            "foo": { 
                "type": "string",
                "description": "This is a string"
            }
        },
        "additionalProperties": false
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (complexSchema ["schema:index:root", """{
        "type":"object",
        "properties":{
            "foo":{
                "type":"string",
                "description": "This is a string"
            }
        }
    }"""])
    
[<Fact>]
let ``Test complex object`` () =
    // This schema can't emit to Pulumi as a single nested object because we need to declare a "complexTypeSpec"
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object",
        "properties": {
            "foo": { 
                "type": "object",
                "properties": {
                    "bar": { "type": "number" }
                },
                "additionalProperties": false
            }
        },
        "additionalProperties": false
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (complexSchema [
        "schema:index:root", """{"type":"object","properties":{"foo":{"$ref":"#/types/schema:index:foo"}}}"""
        "schema:index:foo", """{"type":"object","properties":{"bar":{"type":"number"}}}"""
    ])
    
    Pulumi.Provider.PropertyValue(listToDict [
        "foo", Pulumi.Provider.PropertyValue(listToDict [
            "bar", Pulumi.Provider.PropertyValue(4)
        ])
    ])
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """{"foo":{"bar":4}}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """{}"""

    fromJson """{"foo":{}}"""
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue (listToDict [
        "foo", Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    ]))

    fromJson """{}"""
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)
    
[<Fact>]
let ``Test allOf`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object",
        "properties": {
            "foo": {  "type": "string" }
        },
        "allOf": [ { "properties": { "bar": { "type": "number" } } } ]
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (complexSchema [
        "schema:index:root", """{
            "type":"object",
            "properties":{
                "additionalProperties": {
                    "type": "object",
                    "additionalProperties": {
                        "$ref": "pulumi.json#/Any"
                    }
                },
                "foo":{"type":"string"},
                "bar":{"type":"number"}
            }
        }"""])
    
    Pulumi.Provider.PropertyValue(listToDict [
        "foo", Pulumi.Provider.PropertyValue("hello")
        "bar", Pulumi.Provider.PropertyValue(-123)
    ])
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """{"foo":"hello","bar":-123}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """{}"""

    fromJson """{"foo":"world"}"""
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue (listToDict [
        "foo", Pulumi.Provider.PropertyValue("world")
    ]))

    fromJson """{}"""
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)  
    
[<Fact>]
let ``Test merged refs`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object",
        "properties": {
            "extra": { "type": "number" }
        },
        "$ref": "#/$defs/basicType",
        "$defs": {
            "basicType": { 
                "properties": {
                    "basic": { "type": "number" }
                }
            }
        }
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (complexSchema ["schema:index:root", """{
        "type":"object",
        "properties":{
            "extra":{"type":"number"},
            "basic":{"type":"number"},
            "additionalProperties": {
                "type": "object",
                "additionalProperties": {
                    "$ref": "pulumi.json#/Any"
                }
            }
        }
    }"""])
    
    Pulumi.Provider.PropertyValue(listToDict [
        "extra", Pulumi.Provider.PropertyValue(123)
    ])
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """{"extra":123}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual """{}"""

    fromJson """{"basic":456.789}"""
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue (listToDict [
        "basic", Pulumi.Provider.PropertyValue(456.789)
    ]))

    fromJson """{}"""
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)
    
[<Fact>]
let ``Test oneOf primitives`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "oneOf": [
            { "type": "string" },
            { "type": "number" }
        ]
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion
    
    conversion
    |> conversionToJson    
    |> shouldJsonEqual (simpleSchema """{"oneOf": [
            {"type": "number"},
            {"type": "string"}
        ]
    }""")

    Pulumi.Provider.PropertyValue(45)
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual "45"

    Pulumi.Provider.PropertyValue("hello")
    |> conversion.Writer
    |> toJson
    |> shouldJsonEqual "\"hello\""

    fromJson "123"
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue 123)

    fromJson "\"testing\""
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue "testing")


[<Fact>]
let ``Test properties are Pulumi-ized`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object",
        "properties": {
            "foo": { "type": "string" },
            "Bar": { "type": "string" },
            "foo-bar": { "type": "string" },
            "The-Frober": { "type": "string" },
            "TheMusic": { "type": "string" },
            "aLongName": { "type": "string" },
            "a_snake_name": { "type": "string" },
            "a spacey name": { "type": "string" },
            "a-dashy-name": { "type": "string" },
            "anHTTPClient": { "type": "string" }
        },
        "additionalProperties": false
    }"""
    let conversion = Provider.convertSchema testBaseUri schema.RootElement
    testRoundTrip schema.RootElement conversion
    
    conversion
    |> conversionToJson
    |> shouldJsonEqual (complexSchema ["schema:index:root", """{
        "type":"object",
        "properties":{
            "bar": {
              "type": "string"
            },
            "theFrober": {
              "type": "string"
            },
            "theMusic": {
              "type": "string"
            },
            "aSpaceyName": {
              "type": "string"
            },
            "aDashyName": {
              "type": "string"
            },
            "aLongName": {
              "type": "string"
            },
            "aSnakeName": {
              "type": "string"
            },
            "anHTTPClient": {
              "type": "string"
            },
            "foo": {
              "type": "string"
            },
            "fooBar": {
              "type": "string"
            }
        }
    }"""])
    
    Pulumi.Provider.PropertyValue(listToDict [
        "foo", Pulumi.Provider.PropertyValue("a")
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
    |> shouldEqual (Pulumi.Provider.PropertyValue (listToDict [
        "foo", Pulumi.Provider.PropertyValue("string")
    ]))

    fromJson """{}"""
    |> conversion.Reader
    |> shouldEqual (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)