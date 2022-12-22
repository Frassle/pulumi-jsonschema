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
    /// Additional information about the property's default value, if any.
    /// </summary>
    public sealed class TypesAdditionalPropertiesChoice1Of2PropertiesAdditionalPropertiesDefaultInfo : global::Pulumi.InvokeArgs
    {
        [Input("additionalProperties")]
        private Dictionary<string, object>? _additionalProperties;
        public Dictionary<string, object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new Dictionary<string, object>());
            set => _additionalProperties = value;
        }

        [Input("environment", required: true)]
        private List<string>? _environment;

        /// <summary>
        /// A set of environment variables to probe for a default value.
        /// </summary>
        public List<string> Environment
        {
            get => _environment ?? (_environment = new List<string>());
            set => _environment = value;
        }

        [Input("language")]
        private Dictionary<string, object>? _language;

        /// <summary>
        /// Additional language-specific data about the default value.
        /// </summary>
        public Dictionary<string, object> Language
        {
            get => _language ?? (_language = new Dictionary<string, object>());
            set => _language = value;
        }

        public TypesAdditionalPropertiesChoice1Of2PropertiesAdditionalPropertiesDefaultInfo()
        {
        }
        public static new TypesAdditionalPropertiesChoice1Of2PropertiesAdditionalPropertiesDefaultInfo Empty => new TypesAdditionalPropertiesChoice1Of2PropertiesAdditionalPropertiesDefaultInfo();
    }
}
