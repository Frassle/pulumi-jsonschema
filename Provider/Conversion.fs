namespace JsonSchema

// There are three cases we need to deal with
// Root schemas, that is something like { "type": "string"}
// Child subschemas, that is something like { "items": { "type": "string" } }
// In place subschemas, that is something like { "oneOF": [ ... ] }

// Root and child schemas are in essence the same thing, two functions of:
// JSON Value -> Result<PropertyValue * Annotations, string /* error */>
// PropertyValue -> Result<JSON Value * Annotations, string /* error */>

// In place subschemas are more complex, but can also be seen as just another root schema.
// The issue is given a PropertyValue from an inplace schema + the PropertyValue from the root schema how do you combine them, and how do you combine their types.
// Now the root schema gives an intersection constraint that the inplace schema must be of the same JSON Type, but it might not be the same translated type.
// E.g. { "type": "array", "prefixItems": [{"type": "string"}], "allOf": [ { "minItems": 2 } ] }
// The root schema here would want to translate as a tuple type, but the inplace all off would just assume a plain array.

module Conversion =
    let x = 1

