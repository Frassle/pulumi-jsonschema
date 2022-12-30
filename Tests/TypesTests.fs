module TypesTests

open System
open System.Text.Json.Nodes
open Xunit
open System.Collections.Generic
open System.Collections.Immutable
open JsonSchema.Types

let shouldEqual (json : string) (node : JsonNode) =
    node
    |> Some
    |> Test.toJson
    |> Test.shouldJsonEqual json

[<Fact>]
let ``Test boolean`` () =
    let t = TypeSpec.Primitive PrimitiveType.Boolean

    t.AsSchema Map.empty
    |> shouldEqual """{ "type": "boolean" }"""

[<Fact>]
let ``Test integer`` () =
    let t = TypeSpec.Primitive PrimitiveType.Integer

    t.AsSchema Map.empty
    |> shouldEqual """{ "type": "integer" }"""

[<Fact>]
let ``Test number`` () =
    let t = TypeSpec.Primitive PrimitiveType.Number

    t.AsSchema Map.empty
    |> shouldEqual """{ "type": "number" }"""

[<Fact>]
let ``Test string`` () =
    let t = TypeSpec.Primitive PrimitiveType.String

    t.AsSchema Map.empty
    |> shouldEqual """{ "type": "string" }"""

let arrayTestCases : obj array list = [
    [| TypeSpec.Primitive PrimitiveType.String; """{ "type": "array", "items": { "type": "string" } }""" |]
    [| TypeSpec.Array (TypeSpec.Primitive PrimitiveType.Integer); """{ "type": "array", "items": { "type": "array", "items": { "type": "integer" } } }""" |]
]
[<Theory>]
[<MemberData "arrayTestCases">]
let ``Test array`` (items, expected) =
    let t = TypeSpec.Array items

    t.AsSchema Map.empty
    |> shouldEqual expected

let mapTestCases : obj array list = [
    [| TypeSpec.Primitive PrimitiveType.String; """{ "type": "object", "additionalProperties": { "type": "string" } }""" |]
    [| TypeSpec.Array (TypeSpec.Primitive PrimitiveType.Integer); """{ "type": "object", "additionalProperties": { "type": "array", "items": { "type": "integer" } } }""" |]
]
[<Theory>]
[<MemberData "mapTestCases">]
let ``Test map`` (additionalProperties, expected) =
    let t = TypeSpec.Map additionalProperties

    t.AsSchema Map.empty
    |> shouldEqual expected

[<Theory>]
[<InlineData("pulumi.json#/Any", """{ "$ref": "pulumi.json#/Any" }""")>]
[<InlineData("pulumi.json#/Archive", """{ "$ref": "pulumi.json#/Archive" }""")>]
let ``Test named`` (ref, expected) =
    let t = TypeSpec.Named ref

    t.AsSchema Map.empty
    |> shouldEqual expected

[<Fact>]
let ``Test get complex`` () =
    let complexEnum = ComplexType.Enum {
        Description = None
        Type = PrimitiveType.String
        Enum = Set.ofList [{
            Name = None
            Description = None
            Value = Choice4Of4 "a"
        }]
    }

    let complexObject = ComplexType.Object {
            Description = None
            Required = Set.empty
            Properties = Map.ofList [
                "foo", { 
                    Description = None
                    Const = None
                    Type = TypeSpec.Array (TypeSpec.ComplexType complexEnum)
                }
            ]
        }

    let t = TypeSpec.Map (TypeSpec.ComplexType complexObject)

    t.GetComplexTypes [] Map.empty
    |> Test.shouldEqual (Map.ofList [
        complexEnum, Set.ofList [["items"; "foo"; "additionalProperties"]]
        complexObject, Set.ofList [["additionalProperties"]]
    ])

[<Fact>]
let ``Test get complex with multiple paths`` () =
    let complexEnum = ComplexType.Enum {
        Description = None
        Type = PrimitiveType.String
        Enum = Set.ofList [{
            Name = None
            Description = None
            Value = Choice4Of4 "a"
        }]
    }

    let complexObject = ComplexType.Object {
            Description = None
            Required = Set.empty
            Properties = Map.ofList [
                "foo", { 
                    Description = None
                    Const = None
                    Type = TypeSpec.Array (TypeSpec.ComplexType complexEnum)
                }
                "bar", { 
                    Description = None
                    Const = None
                    Type = TypeSpec.ComplexType complexEnum
                }
            ]
        }

    let t = TypeSpec.Map (TypeSpec.ComplexType complexObject)

    t.GetComplexTypes [] Map.empty
    |> Test.shouldEqual (Map.ofList [
        complexEnum, Set.ofList [["items"; "foo"; "additionalProperties"]; ["bar"; "additionalProperties"]]
        complexObject, Set.ofList [["additionalProperties"]]
    ])