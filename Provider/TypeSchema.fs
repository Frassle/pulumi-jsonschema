module JsonSchema.TypeSchema

open System.Text.Json.Nodes

// These are F# structures that represent Pulumi types and properties
// We use these to handle all the writing out of Pulumi schemas

[<RequireQualifiedAccess>]
type PrimitiveType =
    | Boolean
    | Integer
    | Number
    | String

    member this.JsonValue =
        match this with
        | Boolean -> JsonValue.Create("boolean")
        | Integer -> JsonValue.Create("integer")
        | Number -> JsonValue.Create("number")
        | String -> JsonValue.Create("string")


/// A reference to a type.
[<RequireQualifiedAccess>]
type TypeReference =
    /// A reference to a primitive type.
    | Primitive of PrimitiveType
    /// A reference to an array type.
    | Array of TypeReference
    /// A reference to a map type.
    | Map of TypeReference // Technically optional, but we always fill it in
    /// A reference to a type in this or another document.
    | Named of string // Technically a URI, but that's not F# comparable
    /// A reference to a union type.
    | Union of UnionType

    member this.AsSchema() =
        let schema = JsonObject()
        schema.Add("plain", true)

        match this with
        | Primitive typ -> schema.Add("type", typ.JsonValue)
        | Array typ ->
            schema.Add("type", "array")
            schema.Add("items", typ.AsSchema())
        | Map typ ->
            schema.Add("type", "object")
            schema.Add("additionalProperties", typ.AsSchema())
        | Named uri -> schema.Add("$ref", uri)
        | Union typ ->
            typ.Type |> Option.iter (fun typ -> schema.Add("type", typ.JsonValue))
            let oneOf = JsonArray()
            typ.OneOf |> Set.iter (fun typ -> oneOf.Add(typ.AsSchema()))
            schema.Add("oneOf", oneOf)

        schema


and UnionType =
    {
        /// The underlying primitive type of the union, if any.
        Type: PrimitiveType option
        /// If present, indicates that values of the type may be one of any of the listed types.
        OneOf: Set<TypeReference>
    }

/// Describes an object or resource property.
type PropertyDefinition =
    {
        Type: TypeReference
        /// The description of the property, if any. Interpreted as Markdown.
        Description: string option
        /// The constant value for the property, if any. The type of the value must be assignable to the type of the property.
        Const: Choice<bool, float, string> option
    }

    member this.AsSchema() =
        let schema = this.Type.AsSchema()
        this.Description |> Option.iter (fun d -> schema.Add("description", d))

        match this.Const with
        | None -> ()
        | Some(Choice1Of3 b) -> schema.Add("const", b)
        | Some(Choice2Of3 n) -> schema.Add("const", n)
        | Some(Choice3Of3 s) -> schema.Add("const", s)

        schema

/// Describes an object or enum type.
[<RequireQualifiedAccess>]
type TypeDefinition =
    | Object of ObjectTypeDefinition
    | Enum of EnumTypeDefinition

    /// The description of the type, if any. Interpreted as Markdown.
    member this.Description =
        match this with
        | Object o -> o.Description
        | Enum e -> e.Description

    member this.AsSchema() =
        match this with
        | Object o -> o.AsSchema()
        | Enum e -> e.AsSchema()

/// escribes an object type.
and ObjectTypeDefinition =
    {
        /// The description of the type, if any. Interpreted as Markdown.
        Description: string option
        /// A map from property name to propertySpec that describes the object's properties.
        Properties: Map<string, PropertyDefinition>
        /// A list of the names of an object type's required properties. These properties must be set for inputs and will always be set for outputs.
        Required: Set<string>
    }

    member this.AsSchema() =
        let schema = JsonObject()
        schema.Add("type", "object")
        this.Description |> Option.iter (fun d -> schema.Add("description", d))
        let properties = JsonObject()
        schema.Add("properties", properties)
        this.Properties |> Map.iter (fun k v -> properties.Add(k, v.AsSchema()))

        if not this.Required.IsEmpty then
            let required = JsonArray()
            schema.Add("required", required)
            this.Required |> Set.iter (fun r -> required.Add r)

        schema

/// Describes an enum type.
and EnumTypeDefinition =
    {
        /// The description of the type, if any. Interpreted as Markdown.
        Description: string option
        /// The underlying primitive type of the enum.
        Type: PrimitiveType
        /// The list of possible values for the enum.
        Enum: EnumValueDefinition list
    }

    member this.AsSchema() =
        let schema = JsonObject()
        this.Description |> Option.iter (fun d -> schema.Add("description", d))
        schema.Add("type", this.Type.JsonValue)
        let enum = JsonArray()
        schema.Add("enum", enum)
        this.Enum |> Seq.iter (fun e -> enum.Add(e.AsSchema()))
        schema

and EnumValueDefinition =
    {
        /// If present, overrides the name of the enum value that would usually be derived from the value.
        Name: string option
        /// The description of the enum value, if any. Interpreted as Markdown.
        Description: string option
        /// The enum value itself.
        Value: Choice<bool, int64, float, string>
    }

    member this.AsSchema() =
        let schema = JsonObject()
        this.Name |> Option.iter (fun n -> schema.Add("name", n))
        this.Description |> Option.iter (fun d -> schema.Add("description", d))

        match this.Value with
        | Choice1Of4 b -> schema.Add("value", b)
        | Choice2Of4 i -> schema.Add("value", i)
        | Choice3Of4 n -> schema.Add("value", n)
        | Choice4Of4 s -> schema.Add("value", s)

        schema
