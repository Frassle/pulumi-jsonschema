// *** WARNING: this file was generated by pulumi-language-nodejs. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

import * as pulumi from "@pulumi/pulumi";
import * as inputs from "../types/input";
import * as outputs from "../types/output";
import * as enums from "../types/enums";

/**
 * Describes an object or resource property
 */
export interface AdditionalProperties {
    additionalProperties?: {[key: string]: any};
    choice1Of5?: outputs.Choice1Of2PropertiesAdditionalPropertiesChoice1Of5;
    choice2Of5?: outputs.Choice2Of5;
    choice3Of5?: outputs.MapType;
    choice4Of5?: outputs.NamedType;
    choice5Of5?: outputs.PropertiesAdditionalPropertiesChoice5Of5;
    const?: boolean | number | string;
    default?: boolean | number | string;
    defaultInfo?: outputs.DefaultInfo;
    /**
     * Indicates whether the property is deprecated
     */
    deprecationMessage?: string;
    /**
     * The description of the property, if any. Interpreted as Markdown.
     */
    description?: string;
    /**
     * Additional language-specific data about the property.
     */
    language?: {[key: string]: any};
    /**
     * Indicates that when used as an input, this type does not accept eventual values.
     */
    plain?: boolean;
    /**
     * Specifies whether a change to the property causes its containing resource to be replaced instead of updated (default false).
     */
    replaceOnChanges?: boolean;
    /**
     * Specifies whether the property is secret (default false).
     */
    secret?: boolean;
    /**
     * Indicates that the provider will replace the resource when this property is changed.
     */
    willReplaceOnChanges?: boolean;
}

/**
 * A reference to a primitive type. A primitive type must have only the "type" property set.
 */
export interface AdditionalPropertiesChoice1Of5 {
    type: enums.AdditionalPropertiesChoice1Of5Type;
}

/**
 * A reference to an array type. The "type" property must be set to "array" and the "items" property must be present. No other properties may be present.
 */
export interface AdditionalPropertiesChoice2Of5 {
    items: outputs.TypeSpec;
    type: "array";
}

/**
 * A reference to a map type. The "type" property must be set to "object" and the "additionalProperties" property may be present. No other properties may be present.
 */
export interface AdditionalPropertiesChoice3Of5 {
    additionalProperties?: outputs.TypeSpec;
    type: "object";
}

/**
 * A reference to a union type. The "oneOf" property must be present. The union may additional specify an underlying primitive type via the "type" property and a discriminator via the "discriminator" property. No other properties may be present.
 */
export interface AdditionalPropertiesChoice5Of5 {
    discriminator?: outputs.Discriminator;
    /**
     * If present, indicates that values of the type may be one of any of the listed types
     */
    oneOf: outputs.TypeSpec[];
    type?: enums.OneOf4PropertiesType;
}

/**
 * A reference to a map type. The "type" property must be set to "object" and the "additionalProperties" property may be present. No other properties may be present.
 */
export interface AdditionalPropertiesInputPropertiesAdditionalPropertiesChoice3Of5 {
    additionalProperties?: outputs.InputPropertiesAdditionalProperties;
    type: "object";
}

export interface AliasDefinition {
    additionalProperties?: {[key: string]: any};
    /**
     * The name portion of the alias, if any
     */
    name?: string;
    /**
     * The project portion of the alias, if any
     */
    project?: string;
    /**
     * The type portion of the alias, if any
     */
    type?: string;
}

/**
 * A reference to an array type. The "type" property must be set to "array" and the "items" property must be present. No other properties may be present.
 */
export interface ArrayType {
    items: outputs.VariablesAdditionalProperties;
    type: "array";
}

export interface BaseProvider {
    /**
     * The unqualified name of the package (e.g. "aws", "azure", "gcp", "kubernetes", "random")
     */
    name: string;
    /**
     * The URL to use when downloading the provider plugin binary.
     */
    pluginDownloadURL?: string;
    /**
     * The version of the package. The version must be valid semver.
     */
    version: string;
}

/**
 * A reference to a primitive type. A primitive type must have only the "type" property set.
 */
export interface Choice1Of2PropertiesAdditionalPropertiesChoice1Of5 {
    type: enums.Choice1Of5Type;
}

/**
 * A reference to a primitive type. A primitive type must have only the "type" property set.
 */
export interface Choice1Of5 {
    type: enums.PropertiesType;
}

/**
 * A reference to an array type. The "type" property must be set to "array" and the "items" property must be present. No other properties may be present.
 */
export interface Choice2Of5 {
    items: outputs.AdditionalProperties;
    type: "array";
}

/**
 * A reference to a map type. The "type" property must be set to "object" and the "additionalProperties" property may be present. No other properties may be present.
 */
export interface Choice3Of5 {
    additionalProperties?: outputs.VariablesAdditionalProperties;
    type: "object";
}

/**
 * A reference to a union type. The "oneOf" property must be present. The union may additional specify an underlying primitive type via the "type" property and a discriminator via the "discriminator" property. No other properties may be present.
 */
export interface Choice5Of5 {
    discriminator?: outputs.Discriminator;
    /**
     * If present, indicates that values of the type may be one of any of the listed types
     */
    oneOf: outputs.InputPropertiesAdditionalProperties[];
    type?: enums.InputPropertiesAdditionalPropertiesChoice5Of5Type;
}

/**
 * The package's configuration variables.
 */
export interface Config {
    /**
     * A list of the names of the package's non-required configuration variables.
     */
    defaults?: string[];
    /**
     * A map from variable name to propertySpec that describes a package's configuration variables.
     */
    variables?: {[key: string]: outputs.VariablesAdditionalProperties};
}

/**
 * Additional information about the property's default value, if any.
 */
export interface DefaultInfo {
    additionalProperties?: {[key: string]: any};
    /**
     * A set of environment variables to probe for a default value.
     */
    environment: string[];
    /**
     * Additional language-specific data about the default value.
     */
    language?: {[key: string]: any};
}

/**
 * Informs the consumer of an alternative schema based on the value associated with it
 */
export interface Discriminator {
    additionalProperties?: {[key: string]: any};
    /**
     * An optional object to hold mappings between payload values and schema names or references
     */
    mapping?: {[key: string]: string};
    /**
     * PropertyName is the name of the property in the payload that will hold the discriminator value
     */
    propertyName: string;
}

/**
 * Describes an enum type
 */
export interface EnumTypeDefinition {
    /**
     * The list of possible values for the enum
     */
    enum: outputs.EnumValueDefinition[];
    type: enums.EnumTypeSpecPropertiesType;
}

export interface EnumValueDefinition {
    additionalProperties?: {[key: string]: any};
    /**
     * Indicates whether the value is deprecated.
     */
    deprecationMessage?: string;
    /**
     * The description of the enum value, if any. Interpreted as Markdown.
     */
    description?: string;
    /**
     * If present, overrides the name of the enum value that would usually be derived from the value.
     */
    name?: string;
    value: boolean | number | number | string;
}

/**
 * Describes a function.
 */
export interface FunctionDefinition {
    additionalProperties?: {[key: string]: any};
    /**
     * Indicates whether the function is deprecated
     */
    deprecationMessage?: string;
    /**
     * The description of the function, if any. Interpreted as Markdown.
     */
    description?: string;
    inputs?: outputs.ObjectTypeDetails;
    /**
     * Indicates that the implementation of the function should not be generated from the schema, and is instead provided out-of-band by the package author
     */
    isOverlay?: boolean;
    /**
     * Additional language-specific data about the function.
     */
    language?: {[key: string]: any};
    /**
     * A list of parameter names that determines whether the input bag should be treated as a single argument or as multiple arguments. The list corresponds to the order in which the parameters should be passed to the function.
     */
    multiArgumentInputs?: string[];
    /**
     * unhandled schema: Json.Schema.DescriptionKeyword, Json.Schema.AnyOfKeyword
     */
    outputs?: any;
}

/**
 * Describes an object or resource property
 */
export interface InputPropertiesAdditionalProperties {
    additionalProperties?: {[key: string]: any};
    choice1Of5?: outputs.TypeSpecOneOf0;
    choice2Of5?: outputs.OneOf1;
    choice3Of5?: outputs.AdditionalPropertiesInputPropertiesAdditionalPropertiesChoice3Of5;
    choice4Of5?: outputs.NamedType;
    choice5Of5?: outputs.Choice5Of5;
    const?: boolean | number | string;
    default?: boolean | number | string;
    defaultInfo?: outputs.DefaultInfo;
    /**
     * Indicates whether the property is deprecated
     */
    deprecationMessage?: string;
    /**
     * The description of the property, if any. Interpreted as Markdown.
     */
    description?: string;
    /**
     * Additional language-specific data about the property.
     */
    language?: {[key: string]: any};
    /**
     * Indicates that when used as an input, this type does not accept eventual values.
     */
    plain?: boolean;
    /**
     * Specifies whether a change to the property causes its containing resource to be replaced instead of updated (default false).
     */
    replaceOnChanges?: boolean;
    /**
     * Specifies whether the property is secret (default false).
     */
    secret?: boolean;
    /**
     * Indicates that the provider will replace the resource when this property is changed.
     */
    willReplaceOnChanges?: boolean;
}

/**
 * A reference to an array type. The "type" property must be set to "array" and the "items" property must be present. No other properties may be present.
 */
export interface InputPropertiesAdditionalPropertiesChoice2Of5 {
    items: outputs.PropertyDefinition;
    type: "array";
}

/**
 * A reference to a map type. The "type" property must be set to "object" and the "additionalProperties" property may be present. No other properties may be present.
 */
export interface InputPropertiesAdditionalPropertiesChoice3Of5 {
    additionalProperties?: outputs.PropertyDefinition;
    type: "object";
}

/**
 * Describes an object or resource property
 */
export interface InputsPropertiesAdditionalProperties {
    additionalProperties?: {[key: string]: any};
    choice1Of5?: outputs.Choice1Of5;
    choice2Of5?: outputs.TypeSpecOneOf1;
    choice3Of5?: outputs.TypeSpecOneOf2;
    choice4Of5?: outputs.NamedType;
    choice5Of5?: outputs.TypeSpecOneOf4;
    const?: boolean | number | string;
    default?: boolean | number | string;
    defaultInfo?: outputs.DefaultInfo;
    /**
     * Indicates whether the property is deprecated
     */
    deprecationMessage?: string;
    /**
     * The description of the property, if any. Interpreted as Markdown.
     */
    description?: string;
    /**
     * Additional language-specific data about the property.
     */
    language?: {[key: string]: any};
    /**
     * Indicates that when used as an input, this type does not accept eventual values.
     */
    plain?: boolean;
    /**
     * Specifies whether a change to the property causes its containing resource to be replaced instead of updated (default false).
     */
    replaceOnChanges?: boolean;
    /**
     * Specifies whether the property is secret (default false).
     */
    secret?: boolean;
    /**
     * Indicates that the provider will replace the resource when this property is changed.
     */
    willReplaceOnChanges?: boolean;
}

/**
 * A reference to a map type. The "type" property must be set to "object" and the "additionalProperties" property may be present. No other properties may be present.
 */
export interface MapType {
    additionalProperties?: outputs.AdditionalProperties;
    type: "object";
}

/**
 * Format metadata about this package.
 */
export interface Meta {
    /**
     * A regex that is used by the importer to extract a module name from the module portion of a type token. Packages that use the module format "namespace1/namespace2/.../namespaceN" do not need to specify a format. The regex must define one capturing group that contains the module name, which must be formatted as "namespace1/namespace2/...namespaceN".
     */
    moduleFormat?: string;
    /**
     * Write the package to support the pack command.
     */
    supportPack?: boolean;
}

/**
 * A reference to a type in this or another document. The "$ref" property must be present. The "type" property is ignored if it is present. No other properties may be present.
 */
export interface NamedType {
    /**
     * The URI of the referenced type. For example, the built-in Archive, Asset, and Any
     * types are referenced as "pulumi.json#/Archive", "pulumi.json#/Asset", and "pulumi.json#/Any", respectively.
     * A type from this document is referenced as "#/types/pulumi:type:token".
     * A type from another document is referenced as "path#/types/pulumi:type:token", where path is of the form:
     *   "/provider/vX.Y.Z/schema.json" or "pulumi.json" or "http[s]://example.com/provider/vX.Y.Z/schema.json"
     * A resource from this document is referenced as "#/resources/pulumi:type:token".
     * A resource from another document is referenced as "path#/resources/pulumi:type:token", where path is of the form:
     *   "/provider/vX.Y.Z/schema.json" or "pulumi.json" or "http[s]://example.com/provider/vX.Y.Z/schema.json"
     */
    $ref: string;
    /**
     * ignored; present for compatibility with existing schemas
     */
    type?: string;
}

/**
 * Describes an object type
 */
export interface ObjectTypeDefinition {
    /**
     * A map from property name to propertySpec that describes the object's properties.
     */
    properties?: {[key: string]: outputs.AdditionalProperties};
    /**
     * A list of the names of an object type's required properties. These properties must be set for inputs and will always be set for outputs.
     */
    required?: string[];
    type?: "object";
}

/**
 * Describes an object type
 */
export interface ObjectTypeDetails {
    additionalProperties?: {[key: string]: any};
    /**
     * A map from property name to propertySpec that describes the object's properties.
     */
    properties?: {[key: string]: outputs.InputsPropertiesAdditionalProperties};
    /**
     * A list of the names of an object type's required properties. These properties must be set for inputs and will always be set for outputs.
     */
    required?: string[];
}

/**
 * Describes a resource or component.
 */
export interface ObjectTypeSpec {
    additionalProperties?: {[key: string]: any};
    /**
     * The list of aliases for the resource.
     */
    aliases?: outputs.AliasDefinition[];
    /**
     * Indicates whether the resource is deprecated
     */
    deprecationMessage?: string;
    /**
     * The description of the resource, if any. Interpreted as Markdown.
     */
    description?: string;
    /**
     * A map from property name to propertySpec that describes the resource's input properties.
     */
    inputProperties?: {[key: string]: outputs.PropertyDefinition};
    /**
     * Indicates whether the resource is a component.
     */
    isComponent?: boolean;
    /**
     * Indicates that the implementation of the resource should not be generated from the schema, and is instead provided out-of-band by the package author
     */
    isOverlay?: boolean;
    /**
     * A map from method name to function token that describes the resource's method set.
     */
    methods?: {[key: string]: string};
    /**
     * A map from property name to propertySpec that describes the object's properties.
     */
    properties?: {[key: string]: outputs.PropertiesAdditionalProperties};
    /**
     * A list of the names of an object type's required properties. These properties must be set for inputs and will always be set for outputs.
     */
    required?: string[];
    /**
     * A list of the names of the resource's required input properties.
     */
    requiredInputs?: string[];
    stateInputs?: outputs.ObjectTypeSpec;
}

/**
 * A reference to a primitive type. A primitive type must have only the "type" property set.
 */
export interface OneOf0 {
    type: enums.Type;
}

/**
 * A reference to an array type. The "type" property must be set to "array" and the "items" property must be present. No other properties may be present.
 */
export interface OneOf1 {
    items: outputs.InputPropertiesAdditionalProperties;
    type: "array";
}

/**
 * A reference to a map type. The "type" property must be set to "object" and the "additionalProperties" property may be present. No other properties may be present.
 */
export interface OneOf2 {
    additionalProperties?: outputs.PropertiesAdditionalProperties;
    type: "object";
}

/**
 * A reference to a union type. The "oneOf" property must be present. The union may additional specify an underlying primitive type via the "type" property and a discriminator via the "discriminator" property. No other properties may be present.
 */
export interface OneOf4 {
    discriminator?: outputs.Discriminator;
    /**
     * If present, indicates that values of the type may be one of any of the listed types
     */
    oneOf: outputs.VariablesAdditionalProperties[];
    type?: enums.VariablesAdditionalPropertiesChoice5Of5Type;
}

/**
 * An optional object to define parameterization for the package.
 */
export interface Parameterization {
    baseProvider?: outputs.BaseProvider;
    /**
     * The parameter for the provider.
     */
    parameter?: string;
}

/**
 * A reference to a primitive type. A primitive type must have only the "type" property set.
 */
export interface PrimitiveType {
    type: enums.OneOf0PropertiesType;
}

/**
 * Describes an object or resource property
 */
export interface PropertiesAdditionalProperties {
    additionalProperties?: {[key: string]: any};
    choice1Of5?: outputs.PropertiesAdditionalPropertiesChoice1Of5;
    choice2Of5?: outputs.PropertiesAdditionalPropertiesChoice2Of5;
    choice3Of5?: outputs.OneOf2;
    choice4Of5?: outputs.NamedType;
    choice5Of5?: outputs.ProviderPropertiesAdditionalPropertiesChoice5Of5;
    const?: boolean | number | string;
    default?: boolean | number | string;
    defaultInfo?: outputs.DefaultInfo;
    /**
     * Indicates whether the property is deprecated
     */
    deprecationMessage?: string;
    /**
     * The description of the property, if any. Interpreted as Markdown.
     */
    description?: string;
    /**
     * Additional language-specific data about the property.
     */
    language?: {[key: string]: any};
    /**
     * Indicates that when used as an input, this type does not accept eventual values.
     */
    plain?: boolean;
    /**
     * Specifies whether a change to the property causes its containing resource to be replaced instead of updated (default false).
     */
    replaceOnChanges?: boolean;
    /**
     * Specifies whether the property is secret (default false).
     */
    secret?: boolean;
    /**
     * Indicates that the provider will replace the resource when this property is changed.
     */
    willReplaceOnChanges?: boolean;
}

/**
 * A reference to a primitive type. A primitive type must have only the "type" property set.
 */
export interface PropertiesAdditionalPropertiesChoice1Of5 {
    type: enums.PropertiesAdditionalPropertiesChoice1Of5Type;
}

/**
 * A reference to an array type. The "type" property must be set to "array" and the "items" property must be present. No other properties may be present.
 */
export interface PropertiesAdditionalPropertiesChoice2Of5 {
    items: outputs.PropertiesAdditionalProperties;
    type: "array";
}

/**
 * A reference to a union type. The "oneOf" property must be present. The union may additional specify an underlying primitive type via the "type" property and a discriminator via the "discriminator" property. No other properties may be present.
 */
export interface PropertiesAdditionalPropertiesChoice5Of5 {
    discriminator?: outputs.Discriminator;
    /**
     * If present, indicates that values of the type may be one of any of the listed types
     */
    oneOf: outputs.AdditionalProperties[];
    type?: enums.AdditionalPropertiesChoice5Of5Type;
}

/**
 * Describes an object or resource property
 */
export interface PropertyDefinition {
    additionalProperties?: {[key: string]: any};
    choice1Of5?: outputs.AdditionalPropertiesChoice1Of5;
    choice2Of5?: outputs.InputPropertiesAdditionalPropertiesChoice2Of5;
    choice3Of5?: outputs.InputPropertiesAdditionalPropertiesChoice3Of5;
    choice4Of5?: outputs.NamedType;
    choice5Of5?: outputs.UnionType;
    const?: boolean | number | string;
    default?: boolean | number | string;
    defaultInfo?: outputs.DefaultInfo;
    /**
     * Indicates whether the property is deprecated
     */
    deprecationMessage?: string;
    /**
     * The description of the property, if any. Interpreted as Markdown.
     */
    description?: string;
    /**
     * Additional language-specific data about the property.
     */
    language?: {[key: string]: any};
    /**
     * Indicates that when used as an input, this type does not accept eventual values.
     */
    plain?: boolean;
    /**
     * Specifies whether a change to the property causes its containing resource to be replaced instead of updated (default false).
     */
    replaceOnChanges?: boolean;
    /**
     * Specifies whether the property is secret (default false).
     */
    secret?: boolean;
    /**
     * Indicates that the provider will replace the resource when this property is changed.
     */
    willReplaceOnChanges?: boolean;
}

/**
 * A reference to a union type. The "oneOf" property must be present. The union may additional specify an underlying primitive type via the "type" property and a discriminator via the "discriminator" property. No other properties may be present.
 */
export interface ProviderPropertiesAdditionalPropertiesChoice5Of5 {
    discriminator?: outputs.Discriminator;
    /**
     * If present, indicates that values of the type may be one of any of the listed types
     */
    oneOf: outputs.PropertiesAdditionalProperties[];
    type?: enums.TypeSpecOneOf4PropertiesType;
}

/**
 * A description of the schema for a Pulumi Package
 */
export interface PulumiPackageMetaschema {
    /**
     * Freeform text attribution of derived work, if required.
     */
    attribution?: string;
    config?: outputs.Config;
    /**
     * The description of the package. Descriptions are interpreted as Markdown.
     */
    description?: string;
    /**
     * The human-friendly name of the package.
     */
    displayName?: string;
    /**
     * A map from token to functionSpec that describes the set of functions defined by this package.
     */
    functions?: {[key: string]: outputs.FunctionDefinition};
    /**
     * The package's homepage.
     */
    homepage?: string;
    /**
     * The list of keywords that are associated with the package, if any.
     */
    keywords?: string[];
    /**
     * Additional language-specific data about the package.
     */
    language?: {[key: string]: any};
    /**
     * The name of the license used for the package's contents.
     */
    license?: string;
    /**
     * The URL of the package's logo, if any.
     */
    logoUrl?: string;
    meta?: outputs.Meta;
    /**
     * The unqualified name of the package (e.g. "aws", "azure", "gcp", "kubernetes", "random")
     */
    name: string;
    parameterization?: outputs.Parameterization;
    /**
     * The URL to use when downloading the provider plugin binary.
     */
    pluginDownloadURL?: string;
    provider?: outputs.ObjectTypeSpec;
    /**
     * The name of the person or organization that authored and published the package.
     */
    publisher?: string;
    /**
     * The URL at which the package's sources can be found.
     */
    repository?: string;
    /**
     * A map from type token to resourceSpec that describes the set of resources and components defined by this package.
     */
    resources?: {[key: string]: outputs.ResourceDefinition};
    /**
     * A map from type token to complexTypeSpec that describes the set of complex types (i.e. object, enum) defined by this package.
     */
    types?: {[key: string]: outputs.TypeDefinition};
    /**
     * The version of the package. The version must be valid semver.
     */
    version?: string;
}

/**
 * Describes a resource or component.
 */
export interface ResourceDefinition {
    additionalProperties?: {[key: string]: any};
    /**
     * The list of aliases for the resource.
     */
    aliases?: outputs.AliasDefinition[];
    /**
     * Indicates whether the resource is deprecated
     */
    deprecationMessage?: string;
    /**
     * The description of the resource, if any. Interpreted as Markdown.
     */
    description?: string;
    /**
     * A map from property name to propertySpec that describes the resource's input properties.
     */
    inputProperties?: {[key: string]: outputs.InputPropertiesAdditionalProperties};
    /**
     * Indicates whether the resource is a component.
     */
    isComponent?: boolean;
    /**
     * Indicates that the implementation of the resource should not be generated from the schema, and is instead provided out-of-band by the package author
     */
    isOverlay?: boolean;
    /**
     * A map from method name to function token that describes the resource's method set.
     */
    methods?: {[key: string]: string};
    /**
     * A map from property name to propertySpec that describes the object's properties.
     */
    properties?: {[key: string]: outputs.TypeSpec};
    /**
     * A list of the names of an object type's required properties. These properties must be set for inputs and will always be set for outputs.
     */
    required?: string[];
    /**
     * A list of the names of the resource's required input properties.
     */
    requiredInputs?: string[];
    stateInputs?: outputs.ResourceDefinition;
}

/**
 * Describes an object or enum type.
 */
export interface TypeDefinition {
    additionalProperties?: {[key: string]: any};
    choice1Of2?: outputs.ObjectTypeDefinition;
    choice2Of2?: outputs.EnumTypeDefinition;
    /**
     * The description of the type, if any. Interpreted as Markdown.
     */
    description?: string;
    /**
     * Indicates that the implementation of the type should not be generated from the schema, and is instead provided out-of-band by the package author
     */
    isOverlay?: boolean;
    /**
     * Additional language-specific data about the type.
     */
    language?: {[key: string]: any};
}

/**
 * Describes an object or resource property
 */
export interface TypeSpec {
    additionalProperties?: {[key: string]: any};
    choice1Of5?: outputs.OneOf0;
    choice2Of5?: outputs.AdditionalPropertiesChoice2Of5;
    choice3Of5?: outputs.AdditionalPropertiesChoice3Of5;
    choice4Of5?: outputs.NamedType;
    choice5Of5?: outputs.AdditionalPropertiesChoice5Of5;
    const?: boolean | number | string;
    default?: boolean | number | string;
    defaultInfo?: outputs.DefaultInfo;
    /**
     * Indicates whether the property is deprecated
     */
    deprecationMessage?: string;
    /**
     * The description of the property, if any. Interpreted as Markdown.
     */
    description?: string;
    /**
     * Additional language-specific data about the property.
     */
    language?: {[key: string]: any};
    /**
     * Indicates that when used as an input, this type does not accept eventual values.
     */
    plain?: boolean;
    /**
     * Specifies whether a change to the property causes its containing resource to be replaced instead of updated (default false).
     */
    replaceOnChanges?: boolean;
    /**
     * Specifies whether the property is secret (default false).
     */
    secret?: boolean;
    /**
     * Indicates that the provider will replace the resource when this property is changed.
     */
    willReplaceOnChanges?: boolean;
}

/**
 * A reference to a primitive type. A primitive type must have only the "type" property set.
 */
export interface TypeSpecOneOf0 {
    type: enums.TypeSpecOneOf0PropertiesType;
}

/**
 * A reference to an array type. The "type" property must be set to "array" and the "items" property must be present. No other properties may be present.
 */
export interface TypeSpecOneOf1 {
    items: outputs.InputsPropertiesAdditionalProperties;
    type: "array";
}

/**
 * A reference to a map type. The "type" property must be set to "object" and the "additionalProperties" property may be present. No other properties may be present.
 */
export interface TypeSpecOneOf2 {
    additionalProperties?: outputs.InputsPropertiesAdditionalProperties;
    type: "object";
}

/**
 * A reference to a union type. The "oneOf" property must be present. The union may additional specify an underlying primitive type via the "type" property and a discriminator via the "discriminator" property. No other properties may be present.
 */
export interface TypeSpecOneOf4 {
    discriminator?: outputs.Discriminator;
    /**
     * If present, indicates that values of the type may be one of any of the listed types
     */
    oneOf: outputs.InputsPropertiesAdditionalProperties[];
    type?: enums.PropertiesAdditionalPropertiesChoice5Of5Type;
}

/**
 * A reference to a union type. The "oneOf" property must be present. The union may additional specify an underlying primitive type via the "type" property and a discriminator via the "discriminator" property. No other properties may be present.
 */
export interface UnionType {
    discriminator?: outputs.Discriminator;
    /**
     * If present, indicates that values of the type may be one of any of the listed types
     */
    oneOf: outputs.PropertyDefinition[];
    type?: enums.Choice5Of5Type;
}

/**
 * Describes an object or resource property
 */
export interface VariablesAdditionalProperties {
    additionalProperties?: {[key: string]: any};
    choice1Of5?: outputs.PrimitiveType;
    choice2Of5?: outputs.ArrayType;
    choice3Of5?: outputs.Choice3Of5;
    choice4Of5?: outputs.NamedType;
    choice5Of5?: outputs.OneOf4;
    const?: boolean | number | string;
    default?: boolean | number | string;
    defaultInfo?: outputs.DefaultInfo;
    /**
     * Indicates whether the property is deprecated
     */
    deprecationMessage?: string;
    /**
     * The description of the property, if any. Interpreted as Markdown.
     */
    description?: string;
    /**
     * Additional language-specific data about the property.
     */
    language?: {[key: string]: any};
    /**
     * Indicates that when used as an input, this type does not accept eventual values.
     */
    plain?: boolean;
    /**
     * Specifies whether a change to the property causes its containing resource to be replaced instead of updated (default false).
     */
    replaceOnChanges?: boolean;
    /**
     * Specifies whether the property is secret (default false).
     */
    secret?: boolean;
    /**
     * Indicates that the provider will replace the resource when this property is changed.
     */
    willReplaceOnChanges?: boolean;
}
