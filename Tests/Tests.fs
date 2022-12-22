module Tests

open System
open System.Text.Json
open Xunit
open System.Collections.Generic
open System.Collections.Immutable

[<Fact>]
let ``Test empty`` () =
    let schema = System.Text.Json.JsonDocument.Parse "{}"
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion

    // Pulumi schema doesn't support null, so we say it's an anything but only allow null as a value
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.simpleSchema """{"$ref":"pulumi.json#/Any"}""")

    Pulumi.Provider.PropertyValue.Null
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual "null"

    Test.fromJson "null"
    |> conversion.Reader
    |> Test.shouldEqual Pulumi.Provider.PropertyValue.Null


[<Fact>]
let ``Test null`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "null" 
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion

    // Pulumi schema doesn't support null, so we say it's an anything but only allow null as a value
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.simpleSchema """{"$ref":"pulumi.json#/Any"}""")

    Pulumi.Provider.PropertyValue.Null
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual "null"

    Test.fromJson "null"
    |> conversion.Reader
    |> Test.shouldEqual Pulumi.Provider.PropertyValue.Null

[<Fact>]
let ``Test string enum`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "string",
        "enum": ["info", "warn", "error"]
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.complexSchema [
        "schema:index:root", """{"type":"string","enum":[{"value":"info"},{"value":"warn"},{"value":"error"}]}"""
    ])

    Pulumi.Provider.PropertyValue("info")
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual "\"info\""

    Test.fromJson "\"info\""
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue "info")

    let exc = Assert.Throws<Exception>(fun () ->
        Pulumi.Provider.PropertyValue("badenum")
        |> conversion.Writer
        |> ignore
    )
    exc.Message |> Test.shouldEqual "Expected value to match one of the values specified by the enum"

[<Fact>]
let ``Test array`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "array",
        "items": { "type": "string" }
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.simpleSchema """{"type":"array","items":{"type":"string"}}""")
    
    Test.listToProperty [
        Pulumi.Provider.PropertyValue("a");
        Pulumi.Provider.PropertyValue("b");
    ]
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """["a","b"]"""

    Test.fromJson """["foo","bar"]"""
    |> conversion.Reader
    |> Test.shouldEqual (Test.listToProperty [
        Pulumi.Provider.PropertyValue("foo")
        Pulumi.Provider.PropertyValue("bar")
    ])
    
[<Fact>]
let ``Test empty object`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object" 
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.simpleSchema """{
        "type":"object",
        "additionalProperties": {
            "$ref": "pulumi.json#/Any"
        }
    }""")
    
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual "{}"
    
    Test.dictToProperty [
        "hello", Pulumi.Provider.PropertyValue("a")
        "test", Pulumi.Provider.PropertyValue(123)
    ]
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{"hello": "a", "test": 123}"""

    Test.fromJson "{}"
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)

[<Fact>]
let ``Test object with additional properties`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object",
        "additionalProperties": { "type": "string" }
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.simpleSchema """{"type":"object","additionalProperties":{"type":"string"}}""")
    
    Test.dictToProperty [
        "hello", Pulumi.Provider.PropertyValue("a")
        "test", Pulumi.Provider.PropertyValue("b")
    ]
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{"test":"b","hello":"a"}"""

    Test.fromJson """{"a":"string","b":"number"}"""
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "a", Pulumi.Provider.PropertyValue("string")
        "b", Pulumi.Provider.PropertyValue("number")
    ])

[<Fact>]
let ``Test object with properties`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object",
        "properties": {
            "foo": { "type": "string" }
        },
        "additionalProperties": false
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.complexSchema ["schema:index:root", """{"type":"object","properties":{"foo":{"type":"string"}}}"""])
    
    Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("a")
    ]
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{"foo":"a"}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{}"""

    Test.fromJson """{"foo":"string"}"""
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("string")
    ])

    Test.fromJson """{}"""
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)

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
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.complexSchema ["schema:index:root", """{
        "type":"object",
        "properties":{"foo":{"type":"string"}},
        "required": ["foo"]
    }"""])
    
    Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("a")
    ]
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{"foo":"a"}"""

    
    let exc = Assert.Throws<Exception>(fun () ->
        Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
        |> conversion.Writer
        |> ignore
    )
    exc.Message |> Test.shouldEqual "property 'foo' is required"

    Test.fromJson """{"foo":"string"}"""
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("string")
    ])
    

[<Fact>]
let ``Test object with properties and additionalProperties`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object",
        "properties": {
            "foo": { "type": "string" }
        },
        "additionalProperties": { "type": "number" }
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.complexSchema ["schema:index:root", """{
        "type":"object",
        "properties":{
            "foo":{"type":"string"},
            "additionalProperties":{"type": "object", "additionalProperties": {"type": "number"}}
        }
    }"""])
    
    Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("a")
        "additionalProperties", Test.dictToProperty [
            "bob", Pulumi.Provider.PropertyValue(123)
        ]
    ]
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{"foo":"a","bob":123}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{}"""

    Test.fromJson """{"foo":"string", "other": 54}"""
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("string")
        "additionalProperties", Test.dictToProperty [
            "other", Pulumi.Provider.PropertyValue(54)
        ]
    ])

    Test.fromJson """{}"""
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)

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
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.complexSchema ["schema:index:root", """{"type":"object","properties":{"foo":{"type":"number"}}}"""])
    
    Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue(123)
    ]
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{"foo":123}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{}"""

    Test.fromJson """{"foo":456.789}"""
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue(456.789)
    ])

    Test.fromJson """{}"""
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)    

[<Fact>]
let ``Test simple type union`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": ["boolean", "string"]
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.simpleSchema """{"oneOf": [
            {"type": "boolean"},
            {"type": "string"}
        ]
    }""")

    Pulumi.Provider.PropertyValue(true)
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual "true"

    Pulumi.Provider.PropertyValue("hello")
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual "\"hello\""

    Test.fromJson "true"
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue true)

    Test.fromJson "\"testing\""
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue "testing")

[<Fact>]
let ``Test simple description`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "number",
        "description": "This is a number"
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.simpleSchema """{
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
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.complexSchema ["schema:index:root", """{
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
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.complexSchema [
        "schema:index:root", """{"type":"object","properties":{"foo":{"$ref":"#/types/schema:index:foo"}}}"""
        "schema:index:foo", """{"type":"object","properties":{"bar":{"type":"number"}}}"""
    ])
    
    Test.dictToProperty [
        "foo", Test.dictToProperty [
            "bar", Pulumi.Provider.PropertyValue(4)
        ]
    ]
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{"foo":{"bar":4}}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{}"""

    Test.fromJson """{"foo":{}}"""
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    ])

    Test.fromJson """{}"""
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)
    
[<Fact>]
let ``Test allOf`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object",
        "properties": {
            "foo": {  "type": "string" }
        },
        "allOf": [ { "properties": { "bar": { "type": "number" } } } ]
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.complexSchema [
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
    
    Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("hello")
        "bar", Pulumi.Provider.PropertyValue(-123)
    ]
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{"foo":"hello","bar":-123}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{}"""

    Test.fromJson """{"foo":"world"}"""
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("world")
    ])

    Test.fromJson """{}"""
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)  
    
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
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.complexSchema ["schema:index:root", """{
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
    
    Test.dictToProperty [
        "extra", Pulumi.Provider.PropertyValue(123)
    ]
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{"extra":123}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{}"""

    Test.fromJson """{"basic":456.789}"""
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "basic", Pulumi.Provider.PropertyValue(456.789)
    ])

    Test.fromJson """{}"""
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)
    
[<Fact>]
let ``Test oneOf primitives`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "oneOf": [
            { "type": "string" },
            { "type": "number" }
        ]
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson    
    |> Test.shouldJsonEqual (Test.simpleSchema """{"oneOf": [
            {"type": "number"},
            {"type": "string"}
        ]
    }""")

    Pulumi.Provider.PropertyValue(45)
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual "45"

    Pulumi.Provider.PropertyValue("hello")
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual "\"hello\""

    Test.fromJson "123"
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue 123)

    Test.fromJson "\"testing\""
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue "testing")


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
            "anHTTPClient": { "type": "string" },
            "choice1of2": { "type": "string" }
        },
        "additionalProperties": false
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.complexSchema ["schema:index:root", """{
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
            },
            "choice1Of2": {
              "type": "string"
            }
        }
    }"""])
    
    Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("a")
    ]
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{"foo":"a"}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{}"""

    Test.fromJson """{"foo":"string"}"""
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("string")
    ])

    Test.fromJson """{}"""
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)

[<Fact>]
let ``Test oneOf objects`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "oneOf": [
            { "type": "number" },
            { "type": "array" },
            { "type": "object" }
        ]
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.complexSchema ["schema:index:root", """{
        "type":"object",
        "properties":{
            "choice1Of3": {
              "type": "number"
            },
            "choice2Of3": {
              "type": "array",
              "items": {
                "$ref": "pulumi.json#/Any"
              }
            },
            "choice3Of3": {
              "type": "object",
              "additionalProperties": {
                "$ref": "pulumi.json#/Any"
              }
            }
        }
    }"""])

    Test.fromJson "123"
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "choice1Of3", Pulumi.Provider.PropertyValue(123)
    ])

    Test.fromJson "[\"testing\"]"
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "choice2Of3", Test.listToProperty [
            Pulumi.Provider.PropertyValue("testing")
        ]
    ])

[<Fact>]
let ``Test string pattern with oneOf`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "oneOf": [
            { "type": "string", "pattern": "^\\d+$" },
            { "type": "string", "pattern": "^test$" }
        ]
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.complexSchema ["schema:index:root", """{
        "type":"object",
        "properties":{
            "choice1Of2": {
              "type": "string"
            },
            "choice2Of2": {
              "type": "string"
            }
        }
    }"""])

    Test.dictToProperty [
        "choice1Of2", Pulumi.Provider.PropertyValue("123")
    ]
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual "\"123\""

    Test.dictToProperty [
        "choice2Of2", Pulumi.Provider.PropertyValue("test")
    ]
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual "\"test\""

    Test.fromJson "\"456\""
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "choice1Of2", Pulumi.Provider.PropertyValue("456")
    ])

    Test.fromJson "\"test\""
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "choice2Of2", Pulumi.Provider.PropertyValue("test")
    ])

    let exc = Assert.Throws<exn>(fun () ->
        Test.dictToProperty [
            "choice2Of2", Pulumi.Provider.PropertyValue("hello")
        ]
        |> conversion.Writer
        |> ignore)
    exc.Message |> Test.shouldEqual "The string value was not a match for the indicated regular expression"
    
    let exc = Assert.Throws<exn>(fun () ->
        Test.fromJson "\"bob\""
        |> conversion.Reader
        |> ignore)
    exc.Message |> Test.shouldEqual "Expected 1 matching subschema but found 0"

[<Fact>]
let ``Test inline oneOf`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object",
        "properties": {
            "topKey": { "type": "boolean" }
        },
        "oneOf": [
            {
                "properties": {
                    "keyA": { "type": "string" },
                    "keyB": false
                }
            },
            {
                "properties": {
                    "keyA": false,
                    "keyB": { "type": "string" }
                }
            }
        ]
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.complexSchema [
        "schema:index:oneOf0", """{
            "type": "object",
            "properties": {
                "keyA": { "type": "string" }
            }
        }"""
        "schema:index:oneOf1", """{
            "type": "object",
            "properties": {
                "keyB": { "type": "string" }
            }
        }"""
        "schema:index:root", """{
            "type": "object",
            "properties": {
                "topKey": { "type": "boolean" },
                "additionalProperties": {
                    "type": "object",
                    "additionalProperties": {
                        "$ref": "pulumi.json#/Any"
                    }
                },
                "choice1Of2": {
                    "$ref": "#/types/schema:index:oneOf0"
                },
                "choice2Of2": {
                    "$ref": "#/types/schema:index:oneOf1"
                }
            }
        }"""])

    Test.fromJson """{
        "topKey": true,
        "keyA": "bob"
    }"""
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "topKey", Pulumi.Provider.PropertyValue(true)
        "choice1Of2", Test.dictToProperty [
            "keyA", Pulumi.Provider.PropertyValue("bob")
        ]
    ])

    Test.fromJson """{
        "topKey": true,
        "keyB": "charlie"
    }"""
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "topKey", Pulumi.Provider.PropertyValue(true)
        "choice2Of2", Test.dictToProperty [
            "keyA", Pulumi.Provider.PropertyValue("bob")
        ]
    ])

    Test.fromJson """{"topKey": false }"""
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "topKey", Pulumi.Provider.PropertyValue(false)
    ])
        
[<Fact>]
let ``Test object const property`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "object",
        "properties": {
            "foo": { "type": "string" },
            "bar": { "const": "ba ba black sheep" }
        },
        "additionalProperties": false
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.complexSchema ["schema:index:root", """{
        "type":"object",
        "properties":{
            "foo":{"type":"string"},
            "bar":{"type":"string", "const": "ba ba black sheep"}
        }
    }"""])
    
    Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("a")
    ]
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{"foo":"a"}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """{}"""

    Test.fromJson """{"foo":"string"}"""
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("string")
    ])

    Test.fromJson """{}"""
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)
