module TupleTests

open Xunit
    
[<Fact>]
let ``Test tuple`` () =
    let t = Test.convertSchema """{
        "type": "array",
        "prefixItems": [
            { "type": "string" },
            { "type": "number" }
        ],
        "items": false
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema ["schema:index:root", """{
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
    |> t.ShouldWrite """["a",123]"""

    """["foo",-345]"""
    |> t.ShouldRead (Test.dictToProperty [
        "item1", Pulumi.Provider.PropertyValue("foo")
        "item2", Pulumi.Provider.PropertyValue(-345)
    ])
    
[<Fact>]
let ``Test tuple additionalItems`` () =
    let t = Test.convertSchema """{
        "type": "array",
        "prefixItems": [
            { "type": "string" },
            { "type": "number" }
        ],
        "items": { "type": "boolean" }
    }"""
    
    t.ShouldEqual (Test.complexSchema [
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

    """["hello", 45, true, false]"""
    |> t.ShouldRead (Test.dictToProperty [
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
    |> t.ShouldWrite """["goodbye", 47]"""

    let exc = 
        """[false]"""
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"boolean\" but should be \"string\""

    let exc = 
        """[1.5]"""
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"number\" but should be \"string\""
    
    let exc = 
        Test.dictToProperty [
            "item1", Pulumi.Provider.PropertyValue(2)
        ]
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"integer\" but should be \"string\""
    
    let exc = 
        Test.dictToProperty [
            "item1", Pulumi.Provider.PropertyValue(1)
            "item2", Pulumi.Provider.PropertyValue(2)
        ]
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"integer\" but should be \"string\""

[<Fact>]
let ``Test tuple required items`` () =
    let t = Test.convertSchema """{
        "type": "array",
        "prefixItems": [
            { "type": "string" },
            { "type": "number" }
        ],
        "minItems": 1,
        "items": false
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema ["schema:index:root", """{
        "type":"object",
        "properties":{
            "item1" : { "type":"string" },
            "item2" : { "type":"number" }
        },
        "required": ["item1"]
    }"""])
    
    Test.dictToProperty [
        "item1", Pulumi.Provider.PropertyValue("a")
    ]
    |> t.ShouldWrite """["a"]"""

    """["foo"]"""
    |> t.ShouldRead (Test.dictToProperty [
        "item1", Pulumi.Provider.PropertyValue("foo")
    ])

    let exc = 
        """[]"""
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value has fewer than 1 items"

    let exc = 
        Test.dictToProperty []
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "property 'item1' is required"
