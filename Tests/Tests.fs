module Tests

open System
open System.Text.Json
open Xunit
open System.Collections.Generic
open System.Collections.Immutable

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
    
    t.ShouldEqual (Test.complexSchema ["schema:index:basicType", """{
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
        "schema:index:root", """{
            "type":"object",
            "properties": {
                "foo": {
                    "$ref": "#/types/schema:index:myType"
                }
            }
        }"""
        "schema:index:myType", """{
            "type":"object",
            "properties": {
                "anotherOne": {
                    "$ref": "#/types/schema:index:myType"
                },
                "additionalProperties": {
                  "type": "object",
                  "additionalProperties": {
                    "$ref": "pulumi.json#/Any"
                  }
                }    
            }
        }"""])
    
    Test.dictToProperty [
        "foo", Test.dictToProperty [
            "anotherOne", Test.dictToProperty []
        ]
    ]
    |> t.ShouldWrite """{"foo":{"anotherOne": {}}}"""

    """{"foo":{"anotherOne": {"anotherOne": {}}}}"""
    |> t.ShouldRead (Test.dictToProperty [
        "foo", Test.dictToProperty [
            "anotherOne", Test.dictToProperty [
                "anotherOne", Test.dictToProperty []
            ]
        ]
    ])
    
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{}"""
    |> t.ShouldRead (Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)    