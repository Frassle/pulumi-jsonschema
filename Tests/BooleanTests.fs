module BooleanTests

open Xunit
open Pulumi.Experimental.Provider

[<Fact>]
let ``Test boolean`` () =
    let t =
        Test.convertSchema
            """{
        "type": "boolean" 
    }"""

    t.RoundTrip()

    t.ShouldEqual(t.SimpleSchema """{"type":"boolean"}""")

    PropertyValue(true) |> t.ShouldWrite "true"

    "false" |> t.ShouldRead(PropertyValue false)

    let exc = PropertyValue("foo") |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"string\" but should be \"boolean\""

    let exc = "-1" |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"integer\" but should be \"boolean\""

[<Theory>]
[<InlineData "true">]
[<InlineData "{}">]
let ``Test true`` schema =
    let t = Test.convertSchema schema
    t.RoundTrip()

    t.ShouldEqual(t.SimpleSchema """{"$ref": "pulumi.json#/Any"}""")

    PropertyValue true |> t.ShouldRoundTrip "true"

    Test.listToProperty
        [ PropertyValue "hello"
          PropertyValue false
          PropertyValue 45.1
          Test.dictToProperty [ "x", PropertyValue 1; "y", PropertyValue 2 ] ]
    |> t.ShouldRoundTrip
        """[
        "hello", false, 45.1, { "x": 1, "y": 2 }
    ]"""

[<Fact>]
let ``Test false`` () =
    let t = Test.convertSchema """false"""

    t.ShouldEqual(t.SimpleSchema """{"$ref": "pulumi.json#/Any"}""")

    let exc = PropertyValue("foo") |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "All values fail against the false schema"

    let exc = "-1" |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "All values fail against the false schema"
