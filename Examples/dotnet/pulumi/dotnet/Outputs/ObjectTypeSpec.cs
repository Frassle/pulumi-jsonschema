// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.Pulumi.Outputs
{

    /// <summary>
    /// Describes a resource or component.
    /// </summary>
    [OutputType]
    public sealed class ObjectTypeSpec
    {
        public readonly ImmutableDictionary<string, object>? AdditionalProperties;
        /// <summary>
        /// The list of aliases for the resource.
        /// </summary>
        public readonly ImmutableArray<Outputs.AliasDefinition> Aliases;
        /// <summary>
        /// Indicates whether the resource is deprecated
        /// </summary>
        public readonly string? DeprecationMessage;
        /// <summary>
        /// The description of the resource, if any. Interpreted as Markdown.
        /// </summary>
        public readonly string? Description;
        /// <summary>
        /// A map from property name to propertySpec that describes the resource's input properties.
        /// </summary>
        public readonly ImmutableDictionary<string, Outputs.TypeSpec>? InputProperties;
        /// <summary>
        /// Indicates whether the resource is a component.
        /// </summary>
        public readonly bool? IsComponent;
        /// <summary>
        /// Indicates that the implementation of the resource should not be generated from the schema, and is instead provided out-of-band by the package author
        /// </summary>
        public readonly bool? IsOverlay;
        /// <summary>
        /// A map from method name to function token that describes the resource's method set.
        /// </summary>
        public readonly ImmutableDictionary<string, string>? Methods;
        /// <summary>
        /// A map from property name to propertySpec that describes the object's properties.
        /// </summary>
        public readonly ImmutableDictionary<string, Outputs.ProviderPropertiesAdditionalProperties>? Properties;
        /// <summary>
        /// A list of the names of an object type's required properties. These properties must be set for inputs and will always be set for outputs.
        /// </summary>
        public readonly ImmutableArray<string> Required;
        /// <summary>
        /// A list of the names of the resource's required input properties.
        /// </summary>
        public readonly ImmutableArray<string> RequiredInputs;
        /// <summary>
        /// Describes a resource or component.
        /// </summary>
        public readonly Outputs.ObjectTypeSpec? StateInputs;

        [OutputConstructor]
        private ObjectTypeSpec(
            ImmutableDictionary<string, object>? additionalProperties,

            ImmutableArray<Outputs.AliasDefinition> aliases,

            string? deprecationMessage,

            string? description,

            ImmutableDictionary<string, Outputs.TypeSpec>? inputProperties,

            bool? isComponent,

            bool? isOverlay,

            ImmutableDictionary<string, string>? methods,

            ImmutableDictionary<string, Outputs.ProviderPropertiesAdditionalProperties>? properties,

            ImmutableArray<string> required,

            ImmutableArray<string> requiredInputs,

            Outputs.ObjectTypeSpec? stateInputs)
        {
            AdditionalProperties = additionalProperties;
            Aliases = aliases;
            DeprecationMessage = deprecationMessage;
            Description = description;
            InputProperties = inputProperties;
            IsComponent = isComponent;
            IsOverlay = isOverlay;
            Methods = methods;
            Properties = properties;
            Required = required;
            RequiredInputs = requiredInputs;
            StateInputs = stateInputs;
        }
    }
}
