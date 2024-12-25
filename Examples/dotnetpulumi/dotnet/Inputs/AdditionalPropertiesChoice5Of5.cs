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
    public sealed class AdditionalPropertiesChoice5Of5 : global::Pulumi.InvokeArgs
    {
        [Input("discriminator")]
        public Inputs.Discriminator? Discriminator { get; set; }

        [Input("oneOf", required: true)]
        private List<Inputs.PropertiesAdditionalProperties>? _oneOf;

        /// <summary>
        /// If present, indicates that values of the type may be one of any of the listed types
        /// </summary>
        public List<Inputs.PropertiesAdditionalProperties> OneOf
        {
            get => _oneOf ?? (_oneOf = new List<Inputs.PropertiesAdditionalProperties>());
            set => _oneOf = value;
        }

        [Input("type")]
        public Pulumi.Pulumi.OneOf4PropertiesType? Type { get; set; }

        public AdditionalPropertiesChoice5Of5()
        {
        }
        public static new AdditionalPropertiesChoice5Of5 Empty => new AdditionalPropertiesChoice5Of5();
    }
}
