module ConstTests

open Xunit
open System.Collections.Immutable
open Pulumi.Experimental.Provider

[<Fact>]
let ``Test const string`` () =
    let t =
        Test.convertSchema
            """{
        "const": "hello"
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.SimpleSchema
            """{
        "type":"string",
        "const": "hello"
    }"""
    )

    PropertyValue("hello") |> t.ShouldWrite "\"hello\""

    "\"hello\"" |> t.ShouldRead(PropertyValue "hello")

    let exc = "\"goodbye\"" |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Expected \"hello\""

    let exc = PropertyValue("goodbye") |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Expected \"hello\""

[<Fact>]
let ``Test object const property`` () =
    let t =
        Test.convertSchema
            """{
        "type": "object",
        "properties": {
            "foo": { "type": "string" },
            "bar": { "const": "ba ba black sheep" }
        },
        "additionalProperties": false
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:root",
              """{
        "type":"object",
        "properties":{
            "foo":{"type":"string"},
            "bar":{"type":"string", "const": "ba ba black sheep"}
        }
    }""" ]
    )

    Test.dictToProperty [ "foo", PropertyValue("a") ]
    |> t.ShouldWrite """{"foo":"a"}"""

    // Properties are optional by default
    PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{"foo":"string"}"""
    |> t.ShouldRead(Test.dictToProperty [ "foo", PropertyValue("string") ])

    """{}"""
    |> t.ShouldRead(PropertyValue ImmutableDictionary.Empty)
