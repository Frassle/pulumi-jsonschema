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
    /// A reference to a union type. The "oneOf" property must be present. The union may additional specify an underlying primitive type via the "type" property and a discriminator via the "discriminator" property. No other properties may be present.
    /// </summary>
    public sealed class UnionType3Args : global::Pulumi.ResourceArgs
    {
        [Input("discriminator")]
        public Input<Inputs.PropertySpecOneOf4Discriminator1Args>? Discriminator { get; set; }

        [Input("oneOf", required: true)]
        private InputList<Inputs.PropertyDefinition3Args>? _oneOf;

        /// <summary>
        /// If present, indicates that values of the type may be one of any of the listed types
        /// </summary>
        public InputList<Inputs.PropertyDefinition3Args> OneOf
        {
            get => _oneOf ?? (_oneOf = new InputList<Inputs.PropertyDefinition3Args>());
            set => _oneOf = value;
        }

        [Input("type")]
        public Input<Pulumi.Pulumi.PropertySpecOneOf4Type2>? Type { get; set; }

        public UnionType3Args()
        {
        }
        public static new UnionType3Args Empty => new UnionType3Args();
    }
}
