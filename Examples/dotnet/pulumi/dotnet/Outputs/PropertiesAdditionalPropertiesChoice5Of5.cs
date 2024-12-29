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
    public sealed class PropertiesAdditionalPropertiesChoice5Of5
    {
        public readonly Outputs.Discriminator? Discriminator;
        /// <summary>
        /// If present, indicates that values of the type may be one of any of the listed types
        /// </summary>
        public readonly ImmutableArray<Outputs.ProviderPropertiesAdditionalProperties> OneOf;
        public readonly Pulumi.Pulumi.ProviderPropertiesAdditionalPropertiesChoice5Of5Type? Type;

        [OutputConstructor]
        private PropertiesAdditionalPropertiesChoice5Of5(
            Outputs.Discriminator? discriminator,

            ImmutableArray<Outputs.ProviderPropertiesAdditionalProperties> oneOf,

            Pulumi.Pulumi.ProviderPropertiesAdditionalPropertiesChoice5Of5Type? type)
        {
            Discriminator = discriminator;
            OneOf = oneOf;
            Type = type;
        }
    }
}
