module NumberTests

open Xunit

[<Fact>]
let ``Test number`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "number" 
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.simpleSchema """{"type":"number"}""")

    Pulumi.Provider.PropertyValue(14.512)
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual "14.512"

    Test.fromJson "53.42"
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue 53.42)

    let exc = Assert.Throws<exn>(fun () ->
        Pulumi.Provider.PropertyValue("foo")
        |> conversion.Writer
        |> ignore)
    exc.Message |> Test.shouldEqual "Value is \"string\" but should be \"number\""

    let exc = Assert.Throws<exn>(fun () ->
        Test.fromJson "true"
        |> conversion.Reader
        |> ignore)
    exc.Message |> Test.shouldEqual "Value is \"boolean\" but should be \"number\""

    
[<Fact>]
let ``Test integer`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "integer" 
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.simpleSchema """{"type":"integer"}""")

    Pulumi.Provider.PropertyValue(14)
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual "14"

    Test.fromJson "52"
    |> conversion.Reader
    |> Test.shouldEqual (Pulumi.Provider.PropertyValue 52)

    let exc = Assert.Throws<exn>(fun () ->
        Pulumi.Provider.PropertyValue(123.56)
        |> conversion.Writer
        |> ignore)
    exc.Message |> Test.shouldEqual "Value is \"number\" but should be \"integer\""

    let exc = Assert.Throws<exn>(fun () ->
        Test.fromJson "123.56"
        |> conversion.Reader
        |> ignore)
    exc.Message |> Test.shouldEqual "Value is \"number\" but should be \"integer\""

    let exc = Assert.Throws<exn>(fun () ->
        Pulumi.Provider.PropertyValue("foo")
        |> conversion.Writer
        |> ignore)
    exc.Message |> Test.shouldEqual "Value is \"string\" but should be \"integer\""

    let exc = Assert.Throws<exn>(fun () ->
        Test.fromJson "true"
        |> conversion.Reader
        |> ignore)
    exc.Message |> Test.shouldEqual "Value is \"boolean\" but should be \"integer\""