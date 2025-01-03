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
    public sealed class OneOf0 : global::Pulumi.InvokeArgs
    {
        /// <summary>
        /// The primitive type, if any
        /// </summary>
        [Input("type", required: true)]
        public Pulumi.Pulumi.PropertiesType Type { get; set; }

        public OneOf0()
        {
        }
        public static new OneOf0 Empty => new OneOf0();
    }
}
