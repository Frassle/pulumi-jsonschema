module TupleTests

open Xunit
    
[<Fact>]
let ``Test tuple`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "array",
        "prefixItems": [
            { "type": "string" },
            { "type": "number" }
        ],
        "items": false
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    Test.roundTrip schema.RootElement conversion
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.complexSchema ["schema:index:root", """{
        "type":"object",
        "properties":{
            "item1" : { "type":"string" },
            "item2" : { "type":"number" }
        }
    }"""])
    
    Test.dictToProperty [
        "item1", Pulumi.Provider.PropertyValue("a")
        "item2", Pulumi.Provider.PropertyValue(123)
    ]
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """["a",123]"""

    Test.fromJson """["foo",-345]"""
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "item1", Pulumi.Provider.PropertyValue("foo")
        "item2", Pulumi.Provider.PropertyValue(-345)
    ])
    
[<Fact>]
let ``Test tuple additionalItems`` () =
    let schema = System.Text.Json.JsonDocument.Parse """{
        "type": "array",
        "prefixItems": [
            { "type": "string" },
            { "type": "number" }
        ],
        "items": { "type": "boolean" }
    }"""
    let conversion = Provider.convertSchema Test.baseUri schema.RootElement
    
    conversion
    |> Test.conversionToJson
    |> Test.shouldJsonEqual (Test.complexSchema [
        "schema:index:root", """{
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
        }"""])

    Test.fromJson """["hello", 45, true, false]"""
    |> conversion.Reader
    |> Test.shouldEqual (Test.dictToProperty [
        "item1", Pulumi.Provider.PropertyValue("hello")
        "item2", Pulumi.Provider.PropertyValue(45)
        "rest", Test.listToProperty [
            Pulumi.Provider.PropertyValue(true)
            Pulumi.Provider.PropertyValue(false)
        ]
    ])

    Test.dictToProperty [
        "item1", Pulumi.Provider.PropertyValue("goodbye")
        "item2", Pulumi.Provider.PropertyValue(47)
    ]
    |> conversion.Writer
    |> Test.toJson
    |> Test.shouldJsonEqual """["goodbye", 47]"""

    let exc = Assert.Throws<exn>(fun () ->
        Test.fromJson """[false]"""
        |> conversion.Reader
        |> ignore)
    exc.Message |> Test.shouldEqual "Value is \"boolean\" but should be \"string\""

    let exc = Assert.Throws<exn>(fun () ->
        Test.fromJson """[1.5]"""
        |> conversion.Reader
        |> ignore)
    exc.Message |> Test.shouldEqual "Value is \"number\" but should be \"string\""
    
    let exc = Assert.Throws<exn>(fun () ->
        Test.dictToProperty [
            "item1", Pulumi.Provider.PropertyValue(2)
        ]
        |> conversion.Writer
        |> ignore)
    exc.Message |> Test.shouldEqual "Value is \"integer\" but should be \"string\""
    
    let exc = Assert.Throws<exn>(fun () ->
        Test.dictToProperty [
            "item1", Pulumi.Provider.PropertyValue(1)
            "item2", Pulumi.Provider.PropertyValue(2)
        ]
        |> conversion.Writer
        |> ignore)
    exc.Message |> Test.shouldEqual "Value is \"integer\" but should be \"string\""