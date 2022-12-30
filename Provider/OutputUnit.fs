namespace JsonSchema

/// An output unit, see https://json-schema.org/draft/2020-12/json-schema-core.html#name-output-formatting
type OutputUnit =
    { KeywordLocation: Json.Pointer.JsonPointer
      AbsoluteKeywordLocation: Json.Pointer.JsonPointer option
      InstanceLocation: Json.Pointer.JsonPointer

      AnnotationOrError: Result<System.Text.Json.JsonElement, string>
      NestedResults: OutputUnit list }
