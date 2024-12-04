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
    /// Describes an object or resource property
    /// </summary>
    public sealed class PropertiesAdditionalProperties : global::Pulumi.InvokeArgs
    {
        [Input("additionalProperties")]
        private Dictionary<string, object>? _additionalProperties;
        public Dictionary<string, object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new Dictionary<string, object>());
            set => _additionalProperties = value;
        }

        [Input("choice1Of5")]
        public Inputs.Choice1Of5? Choice1Of5 { get; set; }

        [Input("choice2Of5")]
        public Inputs.ArrayType? Choice2Of5 { get; set; }

        [Input("choice3Of5")]
        public Inputs.MapType? Choice3Of5 { get; set; }

        [Input("choice4Of5")]
        public Inputs.NamedType? Choice4Of5 { get; set; }

        [Input("choice5Of5")]
        public Inputs.AdditionalPropertiesChoice5Of5? Choice5Of5 { get; set; }

        [Input("const")]
        public object? Const { get; set; }

        [Input("default")]
        public object? Default { get; set; }

        [Input("defaultInfo")]
        public Inputs.DefaultInfo? DefaultInfo { get; set; }

        /// <summary>
        /// Indicates whether the property is deprecated
        /// </summary>
        [Input("deprecationMessage")]
        public string? DeprecationMessage { get; set; }

        /// <summary>
        /// The description of the property, if any. Interpreted as Markdown.
        /// </summary>
        [Input("description")]
        public string? Description { get; set; }

        [Input("language")]
        private Dictionary<string, object>? _language;

        /// <summary>
        /// Additional language-specific data about the property.
        /// </summary>
        public Dictionary<string, object> Language
        {
            get => _language ?? (_language = new Dictionary<string, object>());
            set => _language = value;
        }

        /// <summary>
        /// Indicates that when used as an input, this type does not accept eventual values.
        /// </summary>
        [Input("plain")]
        public bool? Plain { get; set; }

        /// <summary>
        /// Specifies whether a change to the property causes its containing resource to be replaced instead of updated (default false).
        /// </summary>
        [Input("replaceOnChanges")]
        public bool? ReplaceOnChanges { get; set; }

        /// <summary>
        /// Specifies whether the property is secret (default false).
        /// </summary>
        [Input("secret")]
        public bool? Secret { get; set; }

        /// <summary>
        /// Indicates that the provider will replace the resource when this property is changed.
        /// </summary>
        [Input("willReplaceOnChanges")]
        public bool? WillReplaceOnChanges { get; set; }

        public PropertiesAdditionalProperties()
        {
        }
        public static new PropertiesAdditionalProperties Empty => new PropertiesAdditionalProperties();
    }
}
