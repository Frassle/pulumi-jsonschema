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
    public sealed class TypeSpecOneOf1 : global::Pulumi.InvokeArgs
    {
        /// <summary>
        /// Describes an object or resource property
        /// </summary>
        [Input("items", required: true)]
        public Inputs.AdditionalProperties Items { get; set; } = null!;

        [Input("type", required: true)]
        public string Type { get; set; } = null!;

        public TypeSpecOneOf1()
        {
        }
        public static new TypeSpecOneOf1 Empty => new TypeSpecOneOf1();
    }
}
