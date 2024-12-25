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
    public sealed class UnionTypeArgs : global::Pulumi.ResourceArgs
    {
        [Input("discriminator")]
        public Input<Inputs.DiscriminatorArgs>? Discriminator { get; set; }

        [Input("oneOf", required: true)]
        private InputList<Inputs.InputsPropertiesAdditionalPropertiesArgs>? _oneOf;

        /// <summary>
        /// If present, indicates that values of the type may be one of any of the listed types
        /// </summary>
        public InputList<Inputs.InputsPropertiesAdditionalPropertiesArgs> OneOf
        {
            get => _oneOf ?? (_oneOf = new InputList<Inputs.InputsPropertiesAdditionalPropertiesArgs>());
            set => _oneOf = value;
        }

        [Input("type")]
        public Input<Pulumi.Pulumi.OneOf4PropertiesType>? Type { get; set; }

        public UnionTypeArgs()
        {
        }
        public static new UnionTypeArgs Empty => new UnionTypeArgs();
    }
}
