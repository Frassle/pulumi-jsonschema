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
    public sealed class PropertiesAdditionalPropertiesChoice3Of5
    {
        public readonly Outputs.AdditionalProperties? AdditionalProperties;
        public readonly string Type;

        [OutputConstructor]
        private PropertiesAdditionalPropertiesChoice3Of5(
            Outputs.AdditionalProperties? additionalProperties,

            string type)
        {
            AdditionalProperties = additionalProperties;
            Type = type;
        }
    }
}
