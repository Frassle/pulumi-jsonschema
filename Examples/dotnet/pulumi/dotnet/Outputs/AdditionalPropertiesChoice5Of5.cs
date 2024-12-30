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
    /// A reference to a union type. The "oneOf" property must be present. The union may additional specify an underlying primitive type via the "type" property and a discriminator via the "discriminator" property. No other properties may be present.
    /// </summary>
    [OutputType]
    public sealed class AdditionalPropertiesChoice5Of5
    {
        /// <summary>
        /// Informs the consumer of an alternative schema based on the value associated with it
        /// </summary>
        public readonly Outputs.Discriminator? Discriminator;
        /// <summary>
        /// If present, indicates that values of the type may be one of any of the listed types
        /// </summary>
        public readonly ImmutableArray<Outputs.InputsPropertiesAdditionalProperties> OneOf;
        /// <summary>
        /// The underlying primitive type of the union, if any
        /// </summary>
        public readonly Pulumi.Pulumi.PropertiesAdditionalPropertiesChoice5Of5Type? Type;

        [OutputConstructor]
        private AdditionalPropertiesChoice5Of5(
            Outputs.Discriminator? discriminator,

            ImmutableArray<Outputs.InputsPropertiesAdditionalProperties> oneOf,

            Pulumi.Pulumi.PropertiesAdditionalPropertiesChoice5Of5Type? type)
        {
            Discriminator = discriminator;
            OneOf = oneOf;
            Type = type;
        }
    }
}
