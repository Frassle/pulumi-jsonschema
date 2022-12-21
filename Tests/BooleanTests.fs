module BooleanTests

open Xunit

[<Fact>]
let ``Test boolean`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "boolean" 
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.simpleSchema """{"type":"boolean"}""")

    Pulumi.Provider.PropertyValue(true)
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual "true"

    Test.fromJson "false"
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue false)

    let exc = Assert.Throws<exn>(fun () ->
        Pulumi.Provider.PropertyValue("foo")
        |> conversion.Writer
        |> ignore)
    exc.Message |> Test.shouldEqual "Value is \"string\" but should be \"boolean\""

    let exc = Assert.Throws<exn>(fun () ->
        Test.fromJson "-1"
        |> conversion.Reader
        |> ignore)
    exc.Message |> Test.shouldEqual "Value is \"integer\" but should be \"boolean\""
