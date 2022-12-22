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
    public sealed class PropertyDefinition3Args : global::Pulumi.ResourceArgs
    {
        [Input("additionalProperties")]
        private InputMap<object>? _additionalProperties;
        public InputMap<object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new InputMap<object>());
            set => _additionalProperties = value;
        }

        [Input("choice1Of5")]
        public Input<Inputs.PrimitiveType2Args>? Choice1Of5 { get; set; }

        [Input("choice2Of5")]
        public Input<Inputs.ArrayType0Args>? Choice2Of5 { get; set; }

        [Input("choice3Of5")]
        public Input<Inputs.MapType6Args>? Choice3Of5 { get; set; }

        [Input("choice4Of5")]
        public Input<Inputs.NamedTypeArgs>? Choice4Of5 { get; set; }

        [Input("choice5Of5")]
        public Input<Inputs.UnionType0Args>? Choice5Of5 { get; set; }

        /// <summary>
        /// The constant value for the property, if any. The type of the value must be assignable to the type of the property.
        /// </summary>
        [Input("const")]
        public object? Const { get; set; }

        /// <summary>
        /// The default value for the property, if any. The type of the value must be assignable to the type of the property.
        /// </summary>
        [Input("default")]
        public object? Default { get; set; }

        [Input("defaultInfo")]
        public Input<Inputs.ResourcesAdditionalPropertiesInputPropertiesAdditionalPropertiesDefaultInfoArgs>? DefaultInfo { get; set; }

        /// <summary>
        /// Indicates whether the property is deprecated
        /// </summary>
        [Input("deprecationMessage")]
        public Input<string>? DeprecationMessage { get; set; }

        /// <summary>
        /// The description of the property, if any. Interpreted as Markdown.
        /// </summary>
        [Input("description")]
        public Input<string>? Description { get; set; }

        [Input("language")]
        private InputMap<object>? _language;

        /// <summary>
        /// Additional language-specific data about the property.
        /// </summary>
        public InputMap<object> Language
        {
            get => _language ?? (_language = new InputMap<object>());
            set => _language = value;
        }

        /// <summary>
        /// Indicates that when used as an input, this type does not accept eventual values.
        /// </summary>
        [Input("plain")]
        public Input<bool>? Plain { get; set; }

        /// <summary>
        /// Specifies whether a change to the property causes its containing resource to be replaced instead of updated (default false).
        /// </summary>
        [Input("replaceOnChanges")]
        public Input<bool>? ReplaceOnChanges { get; set; }

        /// <summary>
        /// Specifies whether the property is secret (default false).
        /// </summary>
        [Input("secret")]
        public Input<bool>? Secret { get; set; }

        /// <summary>
        /// Indicates that the provider will replace the resource when this property is changed.
        /// </summary>
        [Input("willReplaceOnChanges")]
        public Input<bool>? WillReplaceOnChanges { get; set; }

        public PropertyDefinition3Args()
        {
        }
        public static new PropertyDefinition3Args Empty => new PropertyDefinition3Args();
    }
}
