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
let ``Test simple title`` () =
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