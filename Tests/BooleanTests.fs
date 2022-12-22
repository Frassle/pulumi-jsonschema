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
