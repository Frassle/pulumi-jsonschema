module Tests

open System
open System.Text.Json
open Xunit
open System.Collections.Generic
open System.Collections.Immutable

[<Fact>]
let ``Test empty`` () =
    let t = Test.convertSchema "{}"
    t.RoundTrip()

    // Pulumi schema doesn't support null, so we say it's an anything but only allow null as a value
    t.ShouldEqual (Test.simpleSchema """{"$ref":"pulumi.json#/Any"}""")

    Pulumi.Provider.PropertyValue.Null
    |> t.ShouldWrite "null"

    "null"
    |> t.ShouldRead Pulumi.Provider.PropertyValue.Null


[<Fact>]
let ``Test null`` () =
    let t = Test.convertSchema """{
        "type": "null" 
    }"""
    t.RoundTrip()

    // Pulumi schema doesn't support null, so we say it's an anything but only allow null as a value
    t.ShouldEqual (Test.simpleSchema """{"$ref":"pulumi.json#/Any"}""")

    Pulumi.Provider.PropertyValue.Null
    |> t.ShouldWrite "null"

    "null"
    |> t.ShouldRead Pulumi.Provider.PropertyValue.Null

[<Fact>]
let ``Test string enum`` () =
    let t = Test.convertSchema """{
        "type": "string",
        "enum": ["info", "warn", "error"]
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema [
        "schema:index:root", """{"type":"string","enum":[{"value":"info"},{"value":"warn"},{"value":"error"}]}"""
    ])

    Pulumi.Provider.PropertyValue("info")
    |> t.ShouldWrite "\"info\""

    "\"info\""
    |> t.ShouldRead (Pulumi.Provider.PropertyValue "info")

    let exc = 
        Pulumi.Provider.PropertyValue("badenum")
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Expected value to match one of the values specified by the enum"

[<Fact>]
let ``Test array`` () =
    let t = Test.convertSchema """{
        "type": "array",
        "items": { "type": "string" }
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.simpleSchema """{"type":"array","items":{"type":"string"}}""")
    
    Test.listToProperty [
        Pulumi.Provider.PropertyValue("a");
        Pulumi.Provider.PropertyValue("b");
    ]
    |> t.ShouldWrite """["a","b"]"""

    """["foo","bar"]"""
    |> t.ShouldRead (Test.listToProperty [
        Pulumi.Provider.PropertyValue("foo")
        Pulumi.Provider.PropertyValue("bar")
    ])
    
[<Fact>]
let ``Test empty object`` () =
    let t = Test.convertSchema """{
        "type": "object" 
    }""" 
    t.RoundTrip()
    
    t.ShouldEqual (Test.simpleSchema """{
        "type":"object",
        "additionalProperties": {
            "$ref": "pulumi.json#/Any"
        }
    }""")
    
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite "{}"
    
    Test.dictToProperty [
        "hello", Pulumi.Provider.PropertyValue("a")
        "test", Pulumi.Provider.PropertyValue(123)
    ]
    |> t.ShouldWrite """{"hello": "a", "test": 123}"""

    "{}"
    |> t.ShouldRead (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)

[<Fact>]
let ``Test object with additional properties`` () =
    let t = Test.convertSchema """{
        "type": "object",
        "additionalProperties": { "type": "string" }
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.simpleSchema """{"type":"object","additionalProperties":{"type":"string"}}""")
    
    Test.dictToProperty [
        "hello", Pulumi.Provider.PropertyValue("a")
        "test", Pulumi.Provider.PropertyValue("b")
    ]
    |> t.ShouldWrite """{"test":"b","hello":"a"}"""

    """{"a":"string","b":"number"}"""
    |> t.ShouldRead (Test.dictToProperty [
        "a", Pulumi.Provider.PropertyValue("string")
        "b", Pulumi.Provider.PropertyValue("number")
    ])

[<Fact>]
let ``Test object with properties`` () =
    let t = Test.convertSchema """{
        "type": "object",
        "properties": {
            "foo": { "type": "string" }
        },
        "additionalProperties": false
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema ["schema:index:root", """{"type":"object","properties":{"foo":{"type":"string"}}}"""])
    
    Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("a")
    ]
    |> t.ShouldWrite """{"foo":"a"}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{"foo":"string"}"""
    |> t.ShouldRead (Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("string")
    ])

    """{}"""
    |> t.ShouldRead (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)

[<Fact>]
let ``Test object with required properties`` () =
    let t = Test.convertSchema """{
        "type": "object",
        "properties": {
            "foo": { "type": "string" }
        },
        "additionalProperties": false,
        "required": ["foo"]
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema ["schema:index:root", """{
        "type":"object",
        "properties":{"foo":{"type":"string"}},
        "required": ["foo"]
    }"""])
    
    Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("a")
    ]
    |> t.ShouldWrite """{"foo":"a"}"""

    
    let exc =
        Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "property 'foo' is required"

    """{"foo":"string"}"""
    |> t.ShouldRead (Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("string")
    ])

[<Fact>]
let ``Test object with properties and additionalProperties`` () =
    let t = Test.convertSchema """{
        "type": "object",
        "properties": {
            "foo": { "type": "string" }
        },
        "additionalProperties": { "type": "number" }
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema ["schema:index:root", """{
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
    |> t.ShouldWrite """{"foo":"a","bob":123}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{"foo":"string", "other": 54}"""
    |> t.ShouldRead (Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("string")
        "additionalProperties", Test.dictToProperty [
            "other", Pulumi.Provider.PropertyValue(54)
        ]
    ])

    """{}"""
    |> t.ShouldRead (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)

[<Fact>]
let ``Test refs`` () =
    let t = Test.convertSchema """{
        "type": "object",
        "additionalProperties": false,
        "properties": {
            "foo": { "$ref": "#/$defs/myType" }
        },
        "$defs": {
            "myType": { "type": "number" }
        }
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema ["schema:index:root", """{"type":"object","properties":{"foo":{"type":"number"}}}"""])
    
    Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue(123)
    ]
    |> t.ShouldWrite """{"foo":123}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{"foo":456.789}"""
    |> t.ShouldRead (Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue(456.789)
    ])

    """{}"""
    |> t.ShouldRead (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)    

[<Fact>]
let ``Test simple type union`` () =
    let t = Test.convertSchema """{
        "type": ["boolean", "string"]
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.simpleSchema """{"oneOf": [
            {"type": "boolean"},
            {"type": "string"}
        ]
    }""")

    Pulumi.Provider.PropertyValue(true)
    |> t.ShouldWrite "true"

    Pulumi.Provider.PropertyValue("hello")
    |> t.ShouldWrite "\"hello\""

    "true"
    |> t.ShouldRead (Pulumi.Provider.PropertyValue true)

    "\"testing\""
    |> t.ShouldRead (Pulumi.Provider.PropertyValue "testing")

[<Fact>]
let ``Test simple description`` () =
    let t = Test.convertSchema """{
        "type": "number",
        "description": "This is a number"
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.simpleSchema """{
        "type":"number",
        "description": "This is a number"
    }""")

[<Fact>]
let ``Test property description`` () =    
    let t = Test.convertSchema """{
        "type": "object",
        "properties": {
            "foo": { 
                "type": "string",
                "description": "This is a string"
            }
        },
        "additionalProperties": false
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema ["schema:index:root", """{
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
    let t = Test.convertSchema """{
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
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema [
        "schema:index:root", """{"type":"object","properties":{"foo":{"$ref":"#/types/schema:index:foo"}}}"""
        "schema:index:foo", """{"type":"object","properties":{"bar":{"type":"number"}}}"""
    ])
    
    Test.dictToProperty [
        "foo", Test.dictToProperty [
            "bar", Pulumi.Provider.PropertyValue(4)
        ]
    ]
    |> t.ShouldWrite """{"foo":{"bar":4}}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{"foo":{}}"""
    |> t.ShouldRead (Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    ])

    """{}"""
    |> t.ShouldRead (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)
    
[<Fact>]
let ``Test allOf`` () =
    let t = Test.convertSchema """{
        "type": "object",
        "properties": {
            "foo": {  "type": "string" }
        },
        "allOf": [ { "properties": { "bar": { "type": "number" } } } ]
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema [
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
    |> t.ShouldWrite """{"foo":"hello","bar":-123}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{"foo":"world"}"""
    |> t.ShouldRead (Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("world")
    ])

    """{}"""
    |> t.ShouldRead (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)  
    
[<Fact>]
let ``Test merged refs`` () =
    let t = Test.convertSchema """{
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
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema ["schema:index:root", """{
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
    |> t.ShouldWrite """{"extra":123}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{"basic":456.789}"""
    |> t.ShouldRead (Test.dictToProperty [
        "basic", Pulumi.Provider.PropertyValue(456.789)
    ])

    """{}"""
    |> t.ShouldRead (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)
    
[<Fact>]
let ``Test oneOf primitives`` () =
    let t = Test.convertSchema """{
        "oneOf": [
            { "type": "string" },
            { "type": "number" }
        ]
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.simpleSchema """{"oneOf": [
            {"type": "number"},
            {"type": "string"}
        ]
    }""")

    Pulumi.Provider.PropertyValue(45)
    |> t.ShouldWrite "45"

    Pulumi.Provider.PropertyValue("hello")
    |> t.ShouldWrite "\"hello\""

    "123"
    |> t.ShouldRead (Pulumi.Provider.PropertyValue 123)

    "\"testing\""
    |> t.ShouldRead (Pulumi.Provider.PropertyValue "testing")


[<Fact>]
let ``Test properties are Pulumi-ized`` () =
    let t = Test.convertSchema """{
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
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema ["schema:index:root", """{
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
    |> t.ShouldWrite """{"foo":"a"}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{"foo":"string"}"""
    |> t.ShouldRead (Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("string")
    ])

    """{}"""
    |> t.ShouldRead (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)

[<Fact>]
let ``Test oneOf objects`` () =
    let t = Test.convertSchema """{
        "oneOf": [
            { "type": "number" },
            { "type": "array" },
            { "type": "object" }
        ]
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema ["schema:index:root", """{
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

    "123"
    |> t.ShouldRead (Test.dictToProperty [
        "choice1Of3", Pulumi.Provider.PropertyValue(123)
    ])

    "[\"testing\"]"
    |> t.ShouldRead (Test.dictToProperty [
        "choice2Of3", Test.listToProperty [
            Pulumi.Provider.PropertyValue("testing")
        ]
    ])

[<Fact>]
let ``Test string pattern with oneOf`` () =
    let t = Test.convertSchema """{
        "oneOf": [
            { "type": "string", "pattern": "^\\d+$" },
            { "type": "string", "pattern": "^test$" }
        ]
    }"""
    
    t.ShouldEqual (Test.complexSchema ["schema:index:root", """{
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
    |> t.ShouldWrite "\"123\""

    Test.dictToProperty [
        "choice2Of2", Pulumi.Provider.PropertyValue("test")
    ]
    |> t.ShouldWrite "\"test\""

    "\"456\""
    |> t.ShouldRead (Test.dictToProperty [
        "choice1Of2", Pulumi.Provider.PropertyValue("456")
    ])

    "\"test\""
    |> t.ShouldRead (Test.dictToProperty [
        "choice2Of2", Pulumi.Provider.PropertyValue("test")
    ])

    let exc = 
        Test.dictToProperty [
            "choice2Of2", Pulumi.Provider.PropertyValue("hello")
        ]
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "The string value was not a match for the indicated regular expression"
    
    let exc = 
        "\"bob\""
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Expected 1 matching subschema but found 0"

[<Fact>]
let ``Test inline oneOf`` () =
    let t = Test.convertSchema """{
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
    
    t.ShouldEqual (Test.complexSchema [
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

    """{
        "topKey": true,
        "keyA": "bob"
    }"""
    |> t.ShouldRead (Test.dictToProperty [
        "topKey", Pulumi.Provider.PropertyValue(true)
        "choice1Of2", Test.dictToProperty [
            "keyA", Pulumi.Provider.PropertyValue("bob")
        ]
    ])

    """{
        "topKey": true,
        "keyB": "charlie"
    }"""
    |> t.ShouldRead (Test.dictToProperty [
        "topKey", Pulumi.Provider.PropertyValue(true)
        "choice2Of2", Test.dictToProperty [
            "keyB", Pulumi.Provider.PropertyValue("charlie")
        ]
    ])

    """{"keyA": "hello world" }"""
    |> t.ShouldRead (Test.dictToProperty [
        "choice1Of2", Test.dictToProperty [
            "keyA", Pulumi.Provider.PropertyValue("hello world")
        ]
    ])
        
[<Fact>]
let ``Test object const property`` () =
    let t = Test.convertSchema """{
        "type": "object",
        "properties": {
            "foo": { "type": "string" },
            "bar": { "const": "ba ba black sheep" }
        },
        "additionalProperties": false
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema ["schema:index:root", """{
        "type":"object",
        "properties":{
            "foo":{"type":"string"},
            "bar":{"type":"string", "const": "ba ba black sheep"}
        }
    }"""])
    
    Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("a")
    ]
    |> t.ShouldWrite """{"foo":"a"}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{"foo":"string"}"""
    |> t.ShouldRead (Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("string")
    ])

    """{}"""
    |> t.ShouldRead (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)
    
[<Fact>]
let ``Test object with false properties`` () =
    let t = Test.convertSchema """{
        "type": "object",
        "properties": {
            "foo": { "type": "string" },
            "bar": false
        },
        "additionalProperties": true
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema ["schema:index:root", """{
        "type":"object",
        "properties":{
            "foo":{"type":"string"},
            "additionalProperties":{
                "type": "object", 
                "additionalProperties": {
                    "$ref": "pulumi.json#/Any"
                }
            }
        }
    }"""])
    
    Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("a")
        "additionalProperties", Test.dictToProperty [
            "bob", Pulumi.Provider.PropertyValue(123)
        ]
    ]
    |> t.ShouldWrite """{"foo":"a","bob":123}"""
    
    """{"foo":"string", "other": 54}"""
    |> t.ShouldRead (Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue("string")
        "additionalProperties", Test.dictToProperty [
            "other", Pulumi.Provider.PropertyValue(54)
        ]
    ])

    let exc = 
        """{"foo":"string", "bar": true}"""
        |> t.ShouldThrow
    exc.Message |> Test.shouldEqual "All values fail against the false schema"
    
    let exc = 
        Test.dictToProperty [
            "foo", Pulumi.Provider.PropertyValue("a")
            "additionalProperties", Test.dictToProperty [
                "bar", Pulumi.Provider.PropertyValue("anything")
            ]
        ]
        |> t.ShouldThrow
    exc.Message |> Test.shouldEqual "All values fail against the false schema"

[<Fact>]
let ``Test cyclic refs`` () =
    let t = Test.convertSchema """{
        "type": "object",
        "additionalProperties": false,
        "properties": {
            "foo": { "$ref": "#/$defs/myType" }
        },
        "$defs": {
            "myType": {
                "type": "object",
                "properties": {
                    "anotherOne": { "$ref": "#/$defs/myType" } 
                }
            }
        }
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema [
        "schema:index:myType", """{
            "type":"object",
            "properties": {
                "anotherOne": {
                    "$ref": "#/types/schema:index:myType"
                }
            }
    
        }"""
        "schema:index:root", """{
            "type":"object",
            "properties": {
                "foo": {
                    "$ref": "#/types/schema:index:myType"
                }
            }
        }"""])
    
    Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue(123)
    ]
    |> t.ShouldWrite """{"foo":123}"""
    
    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{"foo":456.789}"""
    |> t.ShouldRead (Test.dictToProperty [
        "foo", Pulumi.Provider.PropertyValue(456.789)
    ])

    """{}"""
    |> t.ShouldRead (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)    