module TupleTests

open Xunit
open Pulumi.Experimental.Provider

[<Fact>]
let ``Test tuple`` () =
    let t =
        Test.convertSchema
            """{
        "type": "array",
        "prefixItems": [
            { "type": "string" },
            { "type": "number" }
        ],
        "items": false
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:root",
              """{
        "type":"object",
        "properties":{
            "item1" : { "type":"string" },
            "item2" : { "type":"number" }
        }
    }""" ]
    )

    Test.dictToProperty
        [ "item1", PropertyValue("a")
          "item2", PropertyValue(123) ]
    |> t.ShouldWrite """["a",123]"""

    """["foo",-345]"""
    |> t.ShouldRead(
        Test.dictToProperty
            [ "item1", PropertyValue("foo")
              "item2", PropertyValue(-345) ]
    )

[<Fact>]
let ``Test tuple additionalItems`` () =
    let t =
        Test.convertSchema
            """{
        "type": "array",
        "prefixItems": [
            { "type": "string" },
            { "type": "number" }
        ],
        "items": { "type": "boolean" }
    }"""

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:root",
              """{
            "type": "object",
            "properties": {
                "item1": {
                    "type": "string"
                },
                "item2": {
                    "type": "number"
                },
                "additionalItems": {
                    "type": "array",
                    "items": {
                        "type": "boolean"
                    }
                }
            }
        }""" ]
    )

    """["hello", 45, true, false]"""
    |> t.ShouldRead(
        Test.dictToProperty
            [ "item1", PropertyValue("hello")
              "item2", PropertyValue(45)
              "rest", Test.listToProperty [ PropertyValue(true); PropertyValue(false) ] ]
    )

    Test.dictToProperty
        [ "item1", PropertyValue("goodbye")
          "item2", PropertyValue(47) ]
    |> t.ShouldWrite """["goodbye", 47]"""

    let exc = """[false]""" |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"boolean\" but should be \"string\""

    let exc = """[1.5]""" |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"number\" but should be \"string\""

    let exc =
        Test.dictToProperty [ "item1", PropertyValue(2) ]
        |> t.ShouldThrow<exn>

    exc.Message |> Test.shouldEqual "Value is \"integer\" but should be \"string\""

    let exc =
        Test.dictToProperty
            [ "item1", PropertyValue(1)
              "item2", PropertyValue(2) ]
        |> t.ShouldThrow<exn>

    exc.Message |> Test.shouldEqual "Value is \"integer\" but should be \"string\""

[<Fact>]
let ``Test tuple required items`` () =
    let t =
        Test.convertSchema
            """{
        "type": "array",
        "prefixItems": [
            { "type": "string" },
            { "type": "number" }
        ],
        "minItems": 1,
        "items": false
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:root",
              """{
        "type":"object",
        "properties":{
            "item1" : { "type":"string" },
            "item2" : { "type":"number" }
        },
        "required": ["item1"]
    }""" ]
    )

    Test.dictToProperty [ "item1", PropertyValue("a") ]
    |> t.ShouldWrite """["a"]"""

    """["foo"]"""
    |> t.ShouldRead(Test.dictToProperty [ "item1", PropertyValue("foo") ])

    let exc = """[]""" |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value has fewer than 1 items"

    let exc = Test.dictToProperty [] |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "property 'item1' is required"
