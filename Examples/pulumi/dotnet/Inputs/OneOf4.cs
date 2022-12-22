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
    public sealed class OneOf4 : global::Pulumi.InvokeArgs
    {
        [Input("discriminator")]
        public Inputs.Discriminator? Discriminator { get; set; }

        [Input("oneOf", required: true)]
        private List<Inputs.AdditionalProperties>? _oneOf;

        /// <summary>
        /// If present, indicates that values of the type may be one of any of the listed types
        /// </summary>
        public List<Inputs.AdditionalProperties> OneOf
        {
            get => _oneOf ?? (_oneOf = new List<Inputs.AdditionalProperties>());
            set => _oneOf = value;
        }

        [Input("type")]
        public Pulumi.Pulumi.AdditionalPropertiesPropertiesAdditionalPropertiesChoice5Of5Type? Type { get; set; }

        public OneOf4()
        {
        }
        public static new OneOf4 Empty => new OneOf4();
    }
}
