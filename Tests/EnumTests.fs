module EnumTests

open Xunit
open System.Collections.Immutable
open Pulumi.Experimental.Provider

[<Fact>]
let ``Test string enum`` () =
    let t =
        Test.convertSchema
            """{
        "type": "string",
        "enum": ["info", "warn", "error"]
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        Test.complexSchema
            [ "schema:index:root",
              """{
            "type":"string",
            "enum":[
                {"value":"info"},
                {"value":"warn"},
                {"value":"error"}
            ]
        }""" ]
    )

    PropertyValue("info") |> t.ShouldWrite "\"info\""

    "\"info\"" |> t.ShouldRead(PropertyValue "info")

    let exc = PropertyValue("badenum") |> t.ShouldThrow<exn>

    exc.Message
    |> Test.shouldEqual "Expected value to match one of the values specified by the enum"

[<Fact>]
let ``Test integer enum`` () =
    let t =
        Test.convertSchema
            """{
        "type": "integer",
        "enum": [1, 2, 3]
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        Test.complexSchema
            [ "schema:index:root",
              """{
            "type":"integer",
            "enum":[
                {"value":1},
                {"value":2},
                {"value":3}
            ]
        }""" ]
    )

    PropertyValue(3) |> t.ShouldWrite "3"

    "1" |> t.ShouldRead(PropertyValue 1)

    let exc = PropertyValue(4) |> t.ShouldThrow<exn>

    exc.Message
    |> Test.shouldEqual "Expected value to match one of the values specified by the enum"
