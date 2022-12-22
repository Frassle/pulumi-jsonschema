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
    public sealed class MapType0 : global::Pulumi.InvokeArgs
    {
        [Input("additionalProperties")]
        public Inputs.PropertyDefinition? AdditionalProperties { get; set; }

        [Input("type", required: true)]
        public string Type { get; set; } = null!;

        public MapType0()
        {
        }
        public static new MapType0 Empty => new MapType0();
    }
}
