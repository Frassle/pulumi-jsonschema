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
    public sealed class InputPropertiesAdditionalPropertiesChoice3Of5Args : global::Pulumi.ResourceArgs
    {
        [Input("additionalProperties")]
        public Input<Inputs.ProviderInputPropertiesAdditionalPropertiesArgs>? AdditionalProperties { get; set; }

        [Input("type", required: true)]
        public Input<string> Type { get; set; } = null!;

        public InputPropertiesAdditionalPropertiesChoice3Of5Args()
        {
        }
        public static new InputPropertiesAdditionalPropertiesChoice3Of5Args Empty => new InputPropertiesAdditionalPropertiesChoice3Of5Args();
    }
}