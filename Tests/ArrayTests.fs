module ArrayTests

open Xunit

[<Fact>]
let ``Test array`` () =
    let t = Test.convertSchema """{
        "type": "array",
        "items": { "type": "string" }
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.simpleSchema """{"type":"array","items":{"type":"string"}}""")
    
    Test.listToProperty [
        Pulumi.Provider.PropertyValue("a");
        Pulumi.Provider.PropertyValue("b");
    ]
    |> t.ShouldWrite """["a","b"]"""

    """["foo","bar"]"""
    |> t.ShouldRead (Test.listToProperty [
        Pulumi.Provider.PropertyValue("foo")
        Pulumi.Provider.PropertyValue("bar")
    ])

[<Fact>]
let ``Test true array`` () =
    let t = Test.convertSchema """{
        "type": "array"
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.simpleSchema """{"type":"array","items":{"$ref": "pulumi.json#/Any"}}""")
    
    Test.listToProperty [
        Pulumi.Provider.PropertyValue("a");
        Pulumi.Provider.PropertyValue(5);
    ]
    |> t.ShouldWrite """["a",5]"""

    """["foo",false]"""
    |> t.ShouldRead (Test.listToProperty [
        Pulumi.Provider.PropertyValue("foo")
        Pulumi.Provider.PropertyValue(false)
    ])

[<Fact>]
let ``Test false array`` () =
    let t = Test.convertSchema """{
        "type": "array",
        "items": false
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.simpleSchema """{"type":"array","items":{"$ref": "pulumi.json#/Any"}}""")
    
    Test.listToProperty [
    ]
    |> t.ShouldWrite """[]"""

    """[]"""
    |> t.ShouldRead (Test.listToProperty [])

    let exc = """[1]""" |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "All values fail against the false schema"

    let exc = Test.listToProperty [Pulumi.Provider.PropertyValue(true)] |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "All values fail against the false schema"