module EnumTests

open Xunit
open System.Collections.Immutable

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

    Pulumi.Provider.PropertyValue("info") |> t.ShouldWrite "\"info\""

    "\"info\"" |> t.ShouldRead(Pulumi.Provider.PropertyValue "info")

    let exc = Pulumi.Provider.PropertyValue("badenum") |> t.ShouldThrow<exn>

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

    Pulumi.Provider.PropertyValue(3) |> t.ShouldWrite "3"

    "1" |> t.ShouldRead(Pulumi.Provider.PropertyValue 1)

    let exc = Pulumi.Provider.PropertyValue(4) |> t.ShouldThrow<exn>

    exc.Message
    |> Test.shouldEqual "Expected value to match one of the values specified by the enum"
