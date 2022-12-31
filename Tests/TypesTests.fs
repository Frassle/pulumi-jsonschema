module TypesTests

open System.Text.Json.Nodes
open Xunit
open JsonSchema.Types

let shouldEqual (json: string) (node: JsonNode) =
    node |> Some |> Test.toJson |> Test.shouldJsonEqual json

[<Fact>]
let ``Test boolean`` () =
    let t = TypeReference.Primitive PrimitiveType.Boolean

    t.AsSchema() |> shouldEqual """{ "type": "boolean" }"""

[<Fact>]
let ``Test integer`` () =
    let t = TypeReference.Primitive PrimitiveType.Integer

    t.AsSchema() |> shouldEqual """{ "type": "integer" }"""

[<Fact>]
let ``Test number`` () =
    let t = TypeReference.Primitive PrimitiveType.Number

    t.AsSchema() |> shouldEqual """{ "type": "number" }"""

[<Fact>]
let ``Test string`` () =
    let t = TypeReference.Primitive PrimitiveType.String

    t.AsSchema() |> shouldEqual """{ "type": "string" }"""

let arrayTestCases: obj array list =
    [ [| TypeReference.Primitive PrimitiveType.String
         """{ "type": "array", "items": { "type": "string" } }""" |]
      [| TypeReference.Array(TypeReference.Primitive PrimitiveType.Integer)
         """{ "type": "array", "items": { "type": "array", "items": { "type": "integer" } } }""" |] ]

[<Theory>]
[<MemberData "arrayTestCases">]
let ``Test array`` (items, expected) =
    let t = TypeReference.Array items

    t.AsSchema() |> shouldEqual expected

let mapTestCases: obj array list =
    [ [| TypeReference.Primitive PrimitiveType.String
         """{ "type": "object", "additionalProperties": { "type": "string" } }""" |]
      [| TypeReference.Array(TypeReference.Primitive PrimitiveType.Integer)
         """{ "type": "object", "additionalProperties": { "type": "array", "items": { "type": "integer" } } }""" |] ]

[<Theory>]
[<MemberData "mapTestCases">]
let ``Test map`` (additionalProperties, expected) =
    let t = TypeReference.Map additionalProperties

    t.AsSchema() |> shouldEqual expected

[<Theory>]
[<InlineData("pulumi.json#/Any", """{ "$ref": "pulumi.json#/Any" }""")>]
[<InlineData("pulumi.json#/Archive", """{ "$ref": "pulumi.json#/Archive" }""")>]
let ``Test named`` (ref, expected) =
    let t = TypeReference.Named ref

    t.AsSchema() |> shouldEqual expected
