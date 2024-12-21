module MetaTests

open Xunit

[<Fact>]
let ``Test simple description`` () =
    let t =
        Test.convertSchema
            """{
        "type": "number",
        "description": "This is a number"
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.SimpleSchema
            """{
        "type":"number",
        "description": "This is a number"
    }"""
    )

[<Fact>]
let ``Test property description`` () =
    let t =
        Test.convertSchema
            """{
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

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:root",
              """{
        "type":"object",
        "properties":{
            "foo": {
                "type":"string",
                "description": "This is a string"
            }
        }
    }""" ]
    )

[<Fact>]
let ``Test simple object title`` () =
    let t =
        Test.convertSchema
            """{
        "type": "object",
        "title": "The object",
        "properties": {
            "foo": { "type": "string" }
        },
        "additionalProperties": false
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:theObject",
              """{
        "type":"object",
        "properties":{
            "foo": { "type":"string" }
        }
    }""" ]
    )

[<Fact>]
let ``Test simple enum title`` () =
    let t =
        Test.convertSchema
            """{
        "title": "an enum",
        "type":"string",
        "enum": ["info", "warn", "error"]
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:anEnum",
              """{
        "type": "string",
        "enum": [
            { "value": "info" },
            { "value": "warn" },
            { "value": "error" }
        ]
    }""" ]
    )

[<Fact>]
let ``Test ref title`` () =
    let t =
        Test.convertSchema
            """{
        "type": "object",
        "title": "The Object",
        "properties": {
            "extra": { "type": "number" }
        },
        "$ref": "#/$defs/basicType",
        "$defs": {
            "basicType": { 
                "title": "A basic object",
                "properties": {
                    "basic": { "type": "number" }
                }
            }
        }
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:theObject",
              """{
        "type": "object",
        "properties": {
            "basic": {
                "type": "number"
            },
            "extra": {
                "type": "number"
            },
            "additionalProperties": {
                "type": "object",
                "additionalProperties": {
                    "$ref": "pulumi.json#/Any"
                }
            }
        }
    }""" ]
    )

[<Fact(Skip="allOf title not currently working")>]
let ``Test allOf title`` () =
    let t =
        Test.convertSchema
            """{
        "type": "object",
        "title": "The Object",
        "properties": {
            "extra": { "type": "number" }
        },
        "allOf": [
            {
                "title": "A basic object",
                "properties": {
                    "basic": { "type": "number" }
                }
            }
        ]
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:theObject",
              """{
        "type": "object",
        "properties": {
            "basic": {
                "type": "number"
            },
            "extra": {
                "type": "number"
            },
            "additionalProperties": {
                "type": "object",
                "additionalProperties": {
                    "$ref": "pulumi.json#/Any"
                }
            }
        }
    }""" ]
    )
