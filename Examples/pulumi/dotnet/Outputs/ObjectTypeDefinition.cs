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
    /// Describes an object type
    /// </summary>
    [OutputType]
    public sealed class ObjectTypeDefinition
    {
        /// <summary>
        /// A map from property name to propertySpec that describes the object's properties.
        /// </summary>
        public readonly ImmutableDictionary<string, Outputs.PropertyDefinition2>? Properties;
        /// <summary>
        /// A list of the names of an object type's required properties. These properties must be set for inputs and will always be set for outputs.
        /// </summary>
        public readonly ImmutableArray<string> Required;
        public readonly string? Type;

        [OutputConstructor]
        private ObjectTypeDefinition(
            ImmutableDictionary<string, Outputs.PropertyDefinition2>? properties,

            ImmutableArray<string> required,

            string? type)
        {
            Properties = properties;
            Required = required;
            Type = type;
        }
    }
}
