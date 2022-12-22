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
    public sealed class UnionType1
    {
        public readonly Outputs.ResourcesAdditionalPropertiesInputPropertiesAdditionalPropertiesChoice5Of5Discriminator? Discriminator;
        /// <summary>
        /// If present, indicates that values of the type may be one of any of the listed types
        /// </summary>
        public readonly ImmutableArray<Outputs.PropertyDefinition4> OneOf;
        public readonly Pulumi.Pulumi.ResourcesAdditionalPropertiesPropertiesAdditionalPropertiesChoice5Of5Type? Type;

        [OutputConstructor]
        private UnionType1(
            Outputs.ResourcesAdditionalPropertiesInputPropertiesAdditionalPropertiesChoice5Of5Discriminator? discriminator,

            ImmutableArray<Outputs.PropertyDefinition4> oneOf,

            Pulumi.Pulumi.ResourcesAdditionalPropertiesPropertiesAdditionalPropertiesChoice5Of5Type? type)
        {
            Discriminator = discriminator;
            OneOf = oneOf;
            Type = type;
        }
    }
}