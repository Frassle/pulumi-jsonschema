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
    public sealed class TypeSpecOneOf2Args : global::Pulumi.ResourceArgs
    {
        [Input("additionalProperties")]
        public Input<Inputs.AdditionalPropertiesPropertiesAdditionalPropertiesArgs>? AdditionalProperties { get; set; }

        [Input("type", required: true)]
        public Input<string> Type { get; set; } = null!;

        public TypeSpecOneOf2Args()
        {
        }
        public static new TypeSpecOneOf2Args Empty => new TypeSpecOneOf2Args();
    }
}
