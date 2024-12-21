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
    /// A reference to a map type. The "type" property must be set to "object" and the "additionalProperties" property may be present. No other properties may be present.
    /// </summary>
    [OutputType]
    public sealed class OneOf2
    {
        public readonly Outputs.InputPropertiesAdditionalProperties? AdditionalProperties;
        public readonly string Type;

        [OutputConstructor]
        private OneOf2(
            Outputs.InputPropertiesAdditionalProperties? additionalProperties,

            string type)
        {
            AdditionalProperties = additionalProperties;
            Type = type;
        }
    }
}
