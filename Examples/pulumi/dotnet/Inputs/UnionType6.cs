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
    public sealed class UnionType6 : global::Pulumi.InvokeArgs
    {
        [Input("discriminator")]
        public Inputs.PropertySpecOneOf4Discriminator5? Discriminator { get; set; }

        [Input("oneOf", required: true)]
        private List<Inputs.PropertyDefinition>? _oneOf;

        /// <summary>
        /// If present, indicates that values of the type may be one of any of the listed types
        /// </summary>
        public List<Inputs.PropertyDefinition> OneOf
        {
            get => _oneOf ?? (_oneOf = new List<Inputs.PropertyDefinition>());
            set => _oneOf = value;
        }

        [Input("type")]
        public Pulumi.Pulumi.PropertySpecOneOf4Type1? Type { get; set; }

        public UnionType6()
        {
        }
        public static new UnionType6 Empty => new UnionType6();
    }
}
