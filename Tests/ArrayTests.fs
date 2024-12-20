module ArrayTests

open Xunit

open Pulumi.Experimental.Provider

[<Fact>]
let ``Test array`` () =
    let t =
        Test.convertSchema
            """{
        "type": "array",
        "items": { "type": "string" }
    }"""

    t.RoundTrip()

    t.ShouldEqual(t.SimpleSchema """{"type":"array","items":{"type":"string"}}""")

    Test.listToProperty [ PropertyValue("a"); PropertyValue("b") ]
    |> t.ShouldWrite """["a","b"]"""

    """["foo","bar"]"""
    |> t.ShouldRead(Test.listToProperty [ PropertyValue("foo"); PropertyValue("bar") ])

[<Fact>]
let ``Test true array`` () =
    let t =
        Test.convertSchema
            """{
        "type": "array"
    }"""

    t.RoundTrip()

    t.ShouldEqual(t.SimpleSchema """{"type":"array","items":{"$ref": "pulumi.json#/Any"}}""")

    Test.listToProperty [ PropertyValue("a"); PropertyValue(5) ]
    |> t.ShouldWrite """["a",5]"""

    """["foo",false]"""
    |> t.ShouldRead(Test.listToProperty [ PropertyValue("foo"); PropertyValue(false) ])

[<Fact>]
let ``Test false array`` () =
    let t =
        Test.convertSchema
            """{
        "type": "array",
        "items": false
    }"""

    t.RoundTrip()

    t.ShouldEqual(t.SimpleSchema """{"type":"array","items":{"$ref": "pulumi.json#/Any"}}""")

    Test.listToProperty [] |> t.ShouldWrite """[]"""

    """[]""" |> t.ShouldRead(Test.listToProperty [])

    let exc = """[1]""" |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "All values fail against the false schema"

    let exc =
        Test.listToProperty [ PropertyValue(true) ]
        |> t.ShouldThrow<exn>

    exc.Message |> Test.shouldEqual "All values fail against the false schema"

[<Fact>]
let ``Test array minItems`` () =
    let t =
        Test.convertSchema
            """{
        "type": "array",
        "items": { "type": "string" },
        "minItems": 2
    }"""

    t.RoundTrip()

    t.ShouldEqual(t.SimpleSchema """{"type":"array","items":{"type":"string"}}""")

    Test.listToProperty [ PropertyValue("a"); PropertyValue("b") ]
    |> t.ShouldWrite """["a","b"]"""

    """["foo","bar"]"""
    |> t.ShouldRead(Test.listToProperty [ PropertyValue("foo"); PropertyValue("bar") ])

    let exc =
        Test.listToProperty [ PropertyValue("a") ] |> t.ShouldThrow

    exc.Message |> Test.shouldEqual "Value has fewer than 2 items"

    let exc = """["foo"]""" |> t.ShouldThrow
    exc.Message |> Test.shouldEqual "Value has fewer than 2 items"

[<Fact>]
let ``Test array maxItems`` () =
    let t =
        Test.convertSchema
            """{
        "type": "array",
        "items": { "type": "string" },
        "maxItems": 1
    }"""

    t.RoundTrip()

    t.ShouldEqual(t.SimpleSchema """{"type":"array","items":{"type":"string"}}""")

    Test.listToProperty [ PropertyValue("a") ]
    |> t.ShouldWrite """["a"]"""

    """["foo"]"""
    |> t.ShouldRead(Test.listToProperty [ PropertyValue("foo") ])

    let exc =
        Test.listToProperty [ PropertyValue("a"); PropertyValue("b") ]
        |> t.ShouldThrow

    exc.Message |> Test.shouldEqual "Value has more than 1 items"

    let exc = """["foo", "bar"]""" |> t.ShouldThrow
    exc.Message |> Test.shouldEqual "Value has more than 1 items"
