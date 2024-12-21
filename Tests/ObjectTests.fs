module ObjectTests

open Xunit
open System.Collections.Immutable
open Pulumi.Experimental.Provider

[<Fact>]
let ``Test empty object`` () =
    let t =
        Test.convertSchema
            """{
        "type": "object" 
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.SimpleSchema
            """{
        "type":"object",
        "additionalProperties": {
            "$ref": "pulumi.json#/Any"
        }
    }"""
    )

    PropertyValue(ImmutableDictionary.Empty) |> t.ShouldWrite "{}"

    Test.dictToProperty
        [ "hello", PropertyValue("a")
          "test", PropertyValue(123) ]
    |> t.ShouldWrite """{"hello": "a", "test": 123}"""

    "{}" |> t.ShouldRead(PropertyValue ImmutableDictionary.Empty)

[<Fact>]
let ``Test object with additional properties`` () =
    let t =
        Test.convertSchema
            """{
        "type": "object",
        "additionalProperties": { "type": "string" }
    }"""

    t.RoundTrip()

    t.ShouldEqual(t.SimpleSchema """{"type":"object","additionalProperties":{"type":"string"}}""")

    Test.dictToProperty
        [ "hello", PropertyValue("a")
          "test", PropertyValue("b") ]
    |> t.ShouldWrite """{"test":"b","hello":"a"}"""

    """{"a":"string","b":"number"}"""
    |> t.ShouldRead(
        Test.dictToProperty
            [ "a", PropertyValue("string")
              "b", PropertyValue("number") ]
    )

[<Fact>]
let ``Test object with properties`` () =
    let t =
        Test.convertSchema
            """{
        "type": "object",
        "properties": {
            "foo": { "type": "string" }
        },
        "additionalProperties": false
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.ComplexSchema [ "test:index:root", """{"type":"object","properties":{"foo":{"type":"string"}}}""" ]
    )

    Test.dictToProperty [ "foo", PropertyValue("a") ]
    |> t.ShouldWrite """{"foo":"a"}"""

    // Properties are optional by default
    PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{"foo":"string"}"""
    |> t.ShouldRead(Test.dictToProperty [ "foo", PropertyValue("string") ])

    """{}"""
    |> t.ShouldRead(PropertyValue ImmutableDictionary.Empty)

[<Fact>]
let ``Test object with required properties`` () =
    let t =
        Test.convertSchema
            """{
        "type": "object",
        "properties": {
            "foo": { "type": "string" }
        },
        "additionalProperties": false,
        "required": ["foo"]
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:root",
              """{
        "type":"object",
        "properties":{"foo":{"type":"string"}},
        "required": ["foo"]
    }""" ]
    )

    Test.dictToProperty [ "foo", PropertyValue("a") ]
    |> t.ShouldWrite """{"foo":"a"}"""


    let exc =
        PropertyValue(ImmutableDictionary.Empty) |> t.ShouldThrow<exn>

    exc.Message |> Test.shouldEqual "property 'foo' is required"

    """{"foo":"string"}"""
    |> t.ShouldRead(Test.dictToProperty [ "foo", PropertyValue("string") ])

[<Fact>]
let ``Test object with properties and additionalProperties`` () =
    let t =
        Test.convertSchema
            """{
        "type": "object",
        "properties": {
            "foo": { "type": "string" }
        },
        "additionalProperties": { "type": "number" }
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:root",
              """{
        "type":"object",
        "properties":{
            "foo":{"type":"string"},
            "additionalProperties":{"type": "object", "additionalProperties": {"type": "number"}}
        }
    }""" ]
    )

    Test.dictToProperty
        [ "foo", PropertyValue("a")
          "additionalProperties", Test.dictToProperty [ "bob", PropertyValue(123) ] ]
    |> t.ShouldWrite """{"foo":"a","bob":123}"""

    // Properties are optional by default
    PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{"foo":"string", "other": 54}"""
    |> t.ShouldRead(
        Test.dictToProperty
            [ "foo", PropertyValue("string")
              "additionalProperties", Test.dictToProperty [ "other", PropertyValue(54) ] ]
    )

    """{}"""
    |> t.ShouldRead(PropertyValue ImmutableDictionary.Empty)

[<Fact>]
let ``Test complex object`` () =
    // This schema can't emit to Pulumi as a single nested object because we need to declare a "complexTypeSpec"
    let t =
        Test.convertSchema
            """{
        "type": "object",
        "properties": {
            "foo": { 
                "type": "object",
                "properties": {
                    "bar": { "type": "number" }
                },
                "additionalProperties": false
            }
        },
        "additionalProperties": false
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:root", """{"type":"object","properties":{"foo":{"$ref":"#/types/test:index:foo"}}}"""
              "test:index:foo", """{"type":"object","properties":{"bar":{"type":"number"}}}""" ]
    )

    Test.dictToProperty [ "foo", Test.dictToProperty [ "bar", PropertyValue(4) ] ]
    |> t.ShouldWrite """{"foo":{"bar":4}}"""

    // Properties are optional by default
    PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{"foo":{}}"""
    |> t.ShouldRead(Test.dictToProperty [ "foo", PropertyValue(ImmutableDictionary.Empty) ])

    """{}"""
    |> t.ShouldRead(PropertyValue ImmutableDictionary.Empty)

[<Fact>]
let ``Test properties are Pulumi-ized`` () =
    let t =
        Test.convertSchema
            """{
        "type": "object",
        "properties": {
            "foo": { "type": "string" },
            "Bar": { "type": "string" },
            "foo-bar": { "type": "string" },
            "The-Frober": { "type": "string" },
            "TheMusic": { "type": "string" },
            "aLongName": { "type": "string" },
            "a_snake_name": { "type": "string" },
            "a spacey name": { "type": "string" },
            "a-dashy-name": { "type": "string" },
            "anHTTPClient": { "type": "string" },
            "choice1of2": { "type": "string" }
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
            "foo": { "type": "string" },
            "bar": { "type": "string" },
            "fooBar": { "type": "string" },
            "theFrober": { "type": "string" },
            "theMusic": { "type": "string" },
            "aLongName": { "type": "string" },
            "aSnakeName": { "type": "string" },
            "aSpaceyName": { "type": "string" },
            "aDashyName": { "type": "string" },
            "anHTTPClient": { "type": "string" },
            "choice1Of2": { "type": "string" }
        }
    }""" ]
    )

    Test.dictToProperty [ "foo", PropertyValue("a") ]
    |> t.ShouldWrite """{"foo":"a"}"""

    // Properties are optional by default
    PropertyValue(ImmutableDictionary.Empty)
    |> t.ShouldWrite """{}"""

    """{"foo":"string"}"""
    |> t.ShouldRead(Test.dictToProperty [ "foo", PropertyValue("string") ])

    """{}"""
    |> t.ShouldRead(PropertyValue ImmutableDictionary.Empty)

[<Fact>]
let ``Test object with false properties`` () =
    let t =
        Test.convertSchema
            """{
        "type": "object",
        "properties": {
            "foo": { "type": "string" },
            "bar": false
        },
        "additionalProperties": true
    }"""

    t.RoundTrip()

    t.ShouldEqual(
        t.ComplexSchema
            [ "test:index:root",
              """{
        "type":"object",
        "properties":{
            "foo":{"type":"string"},
            "additionalProperties":{
                "type": "object", 
                "additionalProperties": {
                    "$ref": "pulumi.json#/Any"
                }
            }
        }
    }""" ]
    )

    Test.dictToProperty
        [ "foo", PropertyValue("a")
          "additionalProperties", Test.dictToProperty [ "bob", PropertyValue(123) ] ]
    |> t.ShouldWrite """{"foo":"a","bob":123}"""

    """{"foo":"string", "other": 54}"""
    |> t.ShouldRead(
        Test.dictToProperty
            [ "foo", PropertyValue("string")
              "additionalProperties", Test.dictToProperty [ "other", PropertyValue(54) ] ]
    )

    let exc = """{"foo":"string", "bar": true}""" |> t.ShouldThrow
    exc.Message |> Test.shouldEqual "All values fail against the false schema"

    let exc =
        Test.dictToProperty
            [ "foo", PropertyValue("a")
              "additionalProperties", Test.dictToProperty [ "bar", PropertyValue("anything") ] ]
        |> t.ShouldThrow

    exc.Message |> Test.shouldEqual "All values fail against the false schema"
