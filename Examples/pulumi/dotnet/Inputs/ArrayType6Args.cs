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
    /// A reference to an array type. The "type" property must be set to "array" and the "items" property must be present. No other properties may be present.
    /// </summary>
    public sealed class ArrayType6Args : global::Pulumi.ResourceArgs
    {
        [Input("items", required: true)]
        public Input<Inputs.PropertyDefinition1Args> Items { get; set; } = null!;

        [Input("type", required: true)]
        public Input<string> Type { get; set; } = null!;

        public ArrayType6Args()
        {
        }
        public static new ArrayType6Args Empty => new ArrayType6Args();
    }
}