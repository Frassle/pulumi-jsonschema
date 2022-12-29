module SpecificationTests

open System
open System.Text.Json
open Xunit
open System.Collections.Generic
open System.Collections.Immutable

//[<Fact>]
//let ``Test output structure`` () =
//    let t = Test.convertSchema """{
//      "$id": "https://example.com/polygon",
//      "$schema": "https://json-schema.org/draft/2020-12/schema",
//      "$defs": {
//        "point": {
//          "type": "object",
//          "properties": {
//            "x": { "type": "number" },
//            "y": { "type": "number" }
//          },
//          "additionalProperties": false,
//          "required": [ "x", "y" ]
//        }
//      },
//      "type": "array",
//      "items": { "$ref": "#/$defs/point" },
//      "minItems": 3
//    }"""
//    t.RoundTrip()
//
//    """[
//      {
//        "x": 2.5,
//        "y": 1.3
//      },
//      {
//        "x": 1,
//        "z": 6.7
//      }
//    ]"""
//    |> t.Read
//    |> snd
//    |> Test.shouldEqual {
//        JsonSchema.InstanceLocation = ""
//        JsonSchema.KeywordLocation = ""
//        JsonSchema.AbsoluteKeywordLocation = ""
//        JsonSchema.AnnotationOrError = Error ""
//        JsonSchema.NestedResults = []
//    }
//)    