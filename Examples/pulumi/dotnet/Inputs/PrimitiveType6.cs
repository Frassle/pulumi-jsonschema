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
    /// A reference to a primitive type. A primitive type must have only the "type" property set.
    /// </summary>
    public sealed class PrimitiveType6 : global::Pulumi.InvokeArgs
    {
        [Input("type", required: true)]
        public Pulumi.Pulumi.PropertySpecOneOf0Type5 Type { get; set; }

        public PrimitiveType6()
        {
        }
        public static new PrimitiveType6 Empty => new PrimitiveType6();
    }
}
