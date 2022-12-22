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
    public sealed class TypesAdditionalPropertiesChoice1Of2PropertiesAdditionalPropertiesChoice5Of5Discriminator : global::Pulumi.InvokeArgs
    {
        [Input("additionalProperties")]
        private Dictionary<string, object>? _additionalProperties;
        public Dictionary<string, object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new Dictionary<string, object>());
            set => _additionalProperties = value;
        }

        [Input("mapping")]
        private Dictionary<string, string>? _mapping;

        /// <summary>
        /// an optional object to hold mappings between payload values and schema names or references
        /// </summary>
        public Dictionary<string, string> Mapping
        {
            get => _mapping ?? (_mapping = new Dictionary<string, string>());
            set => _mapping = value;
        }

        /// <summary>
        /// PropertyName is the name of the property in the payload that will hold the discriminator value
        /// </summary>
        [Input("propertyName", required: true)]
        public string PropertyName { get; set; } = null!;

        public TypesAdditionalPropertiesChoice1Of2PropertiesAdditionalPropertiesChoice5Of5Discriminator()
        {
        }
        public static new TypesAdditionalPropertiesChoice1Of2PropertiesAdditionalPropertiesChoice5Of5Discriminator Empty => new TypesAdditionalPropertiesChoice1Of2PropertiesAdditionalPropertiesChoice5Of5Discriminator();
    }
}
