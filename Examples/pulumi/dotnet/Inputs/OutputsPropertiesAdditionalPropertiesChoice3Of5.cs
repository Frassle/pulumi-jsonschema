// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.Pulumi.Inputs
{

    /// <summary>
    /// A reference to a map type. The "type" property must be set to "object" and the "additionalProperties" property may be present. No other properties may be present.
    /// </summary>
    public sealed class OutputsPropertiesAdditionalPropertiesChoice3Of5 : global::Pulumi.InvokeArgs
    {
        [Input("additionalProperties")]
        public Inputs.PropertiesAdditionalProperties? AdditionalProperties { get; set; }

        [Input("type", required: true)]
        public string Type { get; set; } = null!;

        public OutputsPropertiesAdditionalPropertiesChoice3Of5()
        {
        }
        public static new OutputsPropertiesAdditionalPropertiesChoice3Of5 Empty => new OutputsPropertiesAdditionalPropertiesChoice3Of5();
    }
}
