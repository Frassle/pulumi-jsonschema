module Tests

open System
open System.Text.Json
open Xunit
open System.Collections.Generic
open System.Collections.Immutable
open Pulumi.Experimental.Provider

[<Fact>]
let ``Test null`` () =
    let t =
        Test.convertSchema
            """{
        "type": "null" 
    }"""

    t.RoundTrip()

    // Pulumi schema doesn't support null, so we say it's an anything but only allow null as a value
    t.ShouldEqual(t.SimpleSchema """{"$ref":"pulumi.json#/Any"}""")

    PropertyValue.Null |> t.ShouldWrite "null"

    "null" |> t.ShouldRead PropertyValue.Null

[<Fact>]
let ``Test refs`` () =
    let t =
        Test.convertSchema
            """{
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

    t.ShouldEqual(
        t.ComplexSchema [ "test:index:root", """{"type":"object","properties":{"foo":{"type":"number"}}}""" ]
    )

    Test.dictToProperty [ "foo", PropertyValue(123) ]
    |> t.ShouldWrite """{"foo":123}"""

    // Properties are optional by default
    PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{"foo":456.789}"""
    |> t.ShouldRead(Test.dictToProperty [ "foo", PropertyValue(456.789) ])

    """{}"""
    |> t.ShouldRead(PropertyValue ImmutableDictionary.Empty)

[<Fact>]
let ``Test simple type union`` () =
    let t =
        Test.convertSchema
            """{
        "type": ["boolean", "string"]
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.SimpleSchema
            """{"oneOf": [
            {"type": "boolean"},
            {"type": "string"}
        ]
    }"""
    )

    PropertyValue(true) |> t.ShouldWrite "true"

    PropertyValue("hello") |> t.ShouldWrite "\"hello\""

    "true" |> t.ShouldRead(PropertyValue true)

    "\"testing\"" |> t.ShouldRead(PropertyValue "testing")

[<Fact>]
let ``Test allOf`` () =
    let t =
        Test.convertSchema
            """{
        "type": "object",
        "properties": {
            "foo": {  "type": "string" }
        },
        "allOf": [ { "properties": { "bar": { "type": "number" } } } ]
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:root",
              """{
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
        }""" ]
    )

    Test.dictToProperty
        [ "foo", PropertyValue("hello")
          "bar", PropertyValue(-123) ]
    |> t.ShouldWrite """{"foo":"hello","bar":-123}"""

    // Properties are optional by default
    PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{"foo":"world"}"""
    |> t.ShouldRead(Test.dictToProperty [ "foo", PropertyValue("world") ])

    """{}"""
    |> t.ShouldRead(PropertyValue ImmutableDictionary.Empty)

[<Fact>]
let ``Test merged refs`` () =
    let t =
        Test.convertSchema
            """{
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

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:basicType",
              """{
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
    }""" ]
    )

    Test.dictToProperty [ "extra", PropertyValue(123) ]
    |> t.ShouldWrite """{"extra":123}"""

    // Properties are optional by default
    PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{"basic":456.789}"""
    |> t.ShouldRead(Test.dictToProperty [ "basic", PropertyValue(456.789) ])

    """{}"""
    |> t.ShouldRead(PropertyValue ImmutableDictionary.Empty)

[<Fact>]
let ``Test cyclic refs`` () =
    let t =
        Test.convertSchema
            """{
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

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:root",
              """{
            "type":"object",
            "properties": {
                "foo": {
                    "$ref": "#/types/test:index:myType"
                }
            }
        }"""
              "test:index:myType",
              """{
            "type":"object",
            "properties": {
                "anotherOne": {
                    "$ref": "#/types/test:index:myType"
                },
                "additionalProperties": {
                  "type": "object",
                  "additionalProperties": {
                    "$ref": "pulumi.json#/Any"
                  }
                }    
            }
        }""" ]
    )

    Test.dictToProperty [ "foo", Test.dictToProperty [ "anotherOne", Test.dictToProperty [] ] ]
    |> t.ShouldWrite """{"foo":{"anotherOne": {}}}"""

    """{"foo":{"anotherOne": {"anotherOne": {}}}}"""
    |> t.ShouldRead(
        Test.dictToProperty
            [ "foo", Test.dictToProperty [ "anotherOne", Test.dictToProperty [ "anotherOne", Test.dictToProperty [] ] ] ]
    )

    PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{}"""
    |> t.ShouldRead(PropertyValue ImmutableDictionary.Empty)
