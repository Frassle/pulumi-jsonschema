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
    /// Informs the consumer of an alternative schema based on the value associated with it
    /// </summary>
    public sealed class PropertySpecOneOf4Discriminator1Args : global::Pulumi.ResourceArgs
    {
        [Input("additionalProperties")]
        private InputMap<object>? _additionalProperties;
        public InputMap<object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new InputMap<object>());
            set => _additionalProperties = value;
        }

        [Input("mapping")]
        private InputMap<string>? _mapping;

        /// <summary>
        /// an optional object to hold mappings between payload values and schema names or references
        /// </summary>
        public InputMap<string> Mapping
        {
            get => _mapping ?? (_mapping = new InputMap<string>());
            set => _mapping = value;
        }

        /// <summary>
        /// PropertyName is the name of the property in the payload that will hold the discriminator value
        /// </summary>
        [Input("propertyName", required: true)]
        public Input<string> PropertyName { get; set; } = null!;

        public PropertySpecOneOf4Discriminator1Args()
        {
        }
        public static new PropertySpecOneOf4Discriminator1Args Empty => new PropertySpecOneOf4Discriminator1Args();
    }
}
