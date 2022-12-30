module ConstTests

open Xunit
open System.Collections.Immutable

[<Fact>]
let ``Test const string`` () =
    let t =
        Test.convertSchema
            """{
        "const": "hello"
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        Test.simpleSchema
            """{
        "type":"string",
        "const": "hello"
    }"""
    )

    Pulumi.Provider.PropertyValue("hello") |> t.ShouldWrite "\"hello\""

    "\"hello\"" |> t.ShouldRead(Pulumi.Provider.PropertyValue "hello")

    let exc = "\"goodbye\"" |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Expected \"hello\""

    let exc = Pulumi.Provider.PropertyValue("goodbye") |> t.ShouldThrow<exn>
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
        Test.complexSchema
            [ "schema:index:root",
              """{
        "type":"object",
        "properties":{
            "foo":{"type":"string"},
            "bar":{"type":"string", "const": "ba ba black sheep"}
        }
    }""" ]
    )

    Test.dictToProperty [ "foo", Pulumi.Provider.PropertyValue("a") ]
    |> t.ShouldWrite """{"foo":"a"}"""

    // Properties are optional by default
    Pulumi.Provider.PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{"foo":"string"}"""
    |> t.ShouldRead(Test.dictToProperty [ "foo", Pulumi.Provider.PropertyValue("string") ])

    """{}"""
    |> t.ShouldRead(Pulumi.Provider.PropertyValue ImmutableDictionary.Empty)
