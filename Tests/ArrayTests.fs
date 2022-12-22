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