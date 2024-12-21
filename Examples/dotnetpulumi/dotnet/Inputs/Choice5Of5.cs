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
    public sealed class Choice5Of5 : global::Pulumi.InvokeArgs
    {
        [Input("discriminator")]
        public Inputs.Discriminator? Discriminator { get; set; }

        [Input("oneOf", required: true)]
        private List<Inputs.VariablesAdditionalProperties>? _oneOf;

        /// <summary>
        /// If present, indicates that values of the type may be one of any of the listed types
        /// </summary>
        public List<Inputs.VariablesAdditionalProperties> OneOf
        {
            get => _oneOf ?? (_oneOf = new List<Inputs.VariablesAdditionalProperties>());
            set => _oneOf = value;
        }

        [Input("type")]
        public Pulumi.Pulumi.VariablesAdditionalPropertiesChoice5Of5Type? Type { get; set; }

        public Choice5Of5()
        {
        }
        public static new Choice5Of5 Empty => new Choice5Of5();
    }
}
