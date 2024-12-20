module OneOfTests

open System
open System.Text.Json
open Xunit
open System.Collections.Generic
open System.Collections.Immutable
open Pulumi.Experimental.Provider

[<Fact>]
let ``Test oneOf primitives`` () =
    let t =
        Test.convertSchema
            """{
        "oneOf": [
            { "type": "string" },
            { "type": "number" }
        ]
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.SimpleSchema
            """{"oneOf": [
            {"type": "number"},
            {"type": "string"}
        ]
    }"""
    )

    PropertyValue(45) |> t.ShouldWrite "45"

    PropertyValue("hello") |> t.ShouldWrite "\"hello\""

    "123" |> t.ShouldRead(PropertyValue 123)

    "\"testing\"" |> t.ShouldRead(PropertyValue "testing")

[<Fact>]
let ``Test oneOf objects`` () =
    let t =
        Test.convertSchema
            """{
        "oneOf": [
            { "type": "number" },
            { "type": "array" },
            { "type": "object" }
        ]
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:root",
              """{
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
    }""" ]
    )

    "123"
    |> t.ShouldRead(Test.dictToProperty [ "choice1Of3", PropertyValue(123) ])

    "[\"testing\"]"
    |> t.ShouldRead(
        Test.dictToProperty [ "choice2Of3", Test.listToProperty [ PropertyValue("testing") ] ]
    )

[<Fact>]
let ``Test string pattern with oneOf`` () =
    let t =
        Test.convertSchema
            """{
        "oneOf": [
            { "type": "string", "pattern": "^\\d+$" },
            { "type": "string", "pattern": "^test$" }
        ]
    }"""

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:root",
              """{
        "type":"object",
        "properties":{
            "choice1Of2": {
              "type": "string"
            },
            "choice2Of2": {
              "type": "string"
            }
        }
    }""" ]
    )

    Test.dictToProperty [ "choice1Of2", PropertyValue("123") ]
    |> t.ShouldWrite "\"123\""

    Test.dictToProperty [ "choice2Of2", PropertyValue("test") ]
    |> t.ShouldWrite "\"test\""

    "\"456\""
    |> t.ShouldRead(Test.dictToProperty [ "choice1Of2", PropertyValue("456") ])

    "\"test\""
    |> t.ShouldRead(Test.dictToProperty [ "choice2Of2", PropertyValue("test") ])

    let exc =
        Test.dictToProperty [ "choice2Of2", PropertyValue("hello") ]
        |> t.ShouldThrow<exn>

    exc.Message
    |> Test.shouldEqual "The string value was not a match for the indicated regular expression"

    let exc = "\"bob\"" |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Expected 1 matching subschema but found 0"

[<Fact>]
let ``Test inline oneOf`` () =
    let t =
        Test.convertSchema
            """{
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

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:root",
              """{
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
                    "$ref": "#/types/test:index:oneOf0"
                },
                "choice2Of2": {
                    "$ref": "#/types/test:index:oneOf1"
                }
            }
        }"""
              "test:index:oneOf0",
              """{
            "type": "object",
            "properties": {
                "keyA": { "type": "string" }
            }
        }"""
              "test:index:oneOf1",
              """{
            "type": "object",
            "properties": {
                "keyB": { "type": "string" }
            }
        }""" ]
    )

    """{
        "topKey": true,
        "keyA": "bob"
    }"""
    |> t.ShouldRead(
        Test.dictToProperty
            [ "topKey", PropertyValue(true)
              "choice1Of2", Test.dictToProperty [ "keyA", PropertyValue("bob") ] ]
    )

    """{
        "topKey": true,
        "keyB": "charlie"
    }"""
    |> t.ShouldRead(
        Test.dictToProperty
            [ "topKey", PropertyValue(true)
              "choice2Of2", Test.dictToProperty [ "keyB", PropertyValue("charlie") ] ]
    )

    """{"keyA": "hello world" }"""
    |> t.ShouldRead(
        Test.dictToProperty
            [ "choice1Of2", Test.dictToProperty [ "keyA", PropertyValue("hello world") ] ]
    )
