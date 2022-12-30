module BooleanTests

open Xunit

[<Fact>]
let ``Test boolean`` () =
    let t = Test.convertSchema """{
        "type": "boolean" 
    }"""
    t.RoundTrip()

    t.ShouldEqual (Test.simpleSchema """{"type":"boolean"}""")

    Pulumi.Provider.PropertyValue(true)
    |> t.ShouldWrite "true"

    "false"
    |> t.ShouldRead (Pulumi.Provider.PropertyValue false)

    let exc =        
        Pulumi.Provider.PropertyValue("foo")
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"string\" but should be \"boolean\""

    let exc =
        "-1"
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"integer\" but should be \"boolean\""

[<Theory>]
[<InlineData "true">]
[<InlineData "{}">]
let ``Test true`` schema =
    let t = Test.convertSchema schema
    t.RoundTrip()

    t.ShouldEqual (Test.simpleSchema """{"$ref": "pulumi.json#/Any"}""")
    
    Pulumi.Provider.PropertyValue true
    |> t.ShouldRoundTrip "true"

    Test.listToProperty [ 
        Pulumi.Provider.PropertyValue "hello"
        Pulumi.Provider.PropertyValue false
        Pulumi.Provider.PropertyValue 45.1
        Test.dictToProperty [
            "x", Pulumi.Provider.PropertyValue 1
            "y", Pulumi.Provider.PropertyValue 2
        ]
    ]
    |> t.ShouldRoundTrip """[
        "hello", false, 45.1, { "x": 1, "y": 2 }
    ]"""

[<Fact>]
let ``Test false`` () =
    let t = Test.convertSchema """false"""

    t.ShouldEqual (Test.simpleSchema """{"$ref": "pulumi.json#/Any"}""")
    
    let exc =        
        Pulumi.Provider.PropertyValue("foo")
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "All values fail against the false schema"

    let exc =
        "-1"
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "All values fail against the false schema"