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
    public sealed class Inputs
    {
        public readonly ImmutableDictionary<string, object>? AdditionalProperties;
        /// <summary>
        /// A map from property name to propertySpec that describes the object's properties.
        /// </summary>
        public readonly ImmutableDictionary<string, Outputs.TypeSpec>? Properties;
        /// <summary>
        /// A list of the names of an object type's required properties. These properties must be set for inputs and will always be set for outputs.
        /// </summary>
        public readonly ImmutableArray<string> Required;

        [OutputConstructor]
        private Inputs(
            ImmutableDictionary<string, object>? additionalProperties,

            ImmutableDictionary<string, Outputs.TypeSpec>? properties,

            ImmutableArray<string> required)
        {
            AdditionalProperties = additionalProperties;
            Properties = properties;
            Required = required;
        }
    }
}