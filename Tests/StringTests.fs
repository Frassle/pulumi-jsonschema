module StringTests

open Xunit

[<Fact>]
let ``Test plain string`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "string" 
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.simpleSchema """{"type":"string"}""")

    Pulumi.Provider.PropertyValue("test")
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual "\"test\""

    Test.fromJson "\"test\""
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue "test")

[<Fact>]
let ``Test string pattern`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "string",
        "pattern": "^\\d+$"
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.simpleSchema """{"type":"string"}""")

    Pulumi.Provider.PropertyValue("123")
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual "\"123\""

    Test.fromJson "\"456\""
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue "456")

    let exc = Assert.Throws<exn>(fun () ->
        Pulumi.Provider.PropertyValue("hello")
        |> conversion.Writer
        |> ignore)
    exc.Message |> Test.shouldEqual "The string value was not a match for the indicated regular expression"
    
    let exc = Assert.Throws<exn>(fun () ->
        Test.fromJson "\"bob\""
        |> conversion.Reader
        |> ignore)
    exc.Message |> Test.shouldEqual "The string value was not a match for the indicated regular expression"