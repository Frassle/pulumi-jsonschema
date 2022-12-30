module MetaTests

open Xunit

[<Fact>]
let ``Test simple description`` () =
    let t = Test.convertSchema """{
        "type": "number",
        "description": "This is a number"
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.simpleSchema """{
        "type":"number",
        "description": "This is a number"
    }""")

[<Fact>]
let ``Test property description`` () =    
    let t = Test.convertSchema """{
        "type": "object",
        "properties": {
            "foo": { 
                "type": "string",
                "description": "This is a string"
            }
        },
        "additionalProperties": false
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema ["schema:index:root", """{
        "type":"object",
        "properties":{
            "foo": {
                "type":"string",
                "description": "This is a string"
            }
        }
    }"""])

[<Fact>]
let ``Test simple object title`` () =
    let t = Test.convertSchema """{
        "type": "object",
        "title": "The object",
        "properties": {
            "foo": { "type": "string" }
        },
        "additionalProperties": false
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema ["schema:index:theObject", """{
        "type":"object",
        "properties":{
            "foo": { "type":"string" }
        }
    }"""])

[<Fact>]
let ``Test simple enum title`` () =
    let t = Test.convertSchema """{
        "title": "an enum",
        "type":"string",
        "enum": ["info", "warn", "error"]
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.complexSchema ["schema:index:anEnum", """{
        "type": "string",
        "enum": [
            { "value": "info" },
            { "value": "warn" },
            { "value": "error" }
        ]
    }"""])