// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.Pulumi.Outputs
{

    /// <summary>
    /// Describes an object or resource property
    /// </summary>
    [OutputType]
    public sealed class AdditionalProperties
    {
        public readonly ImmutableDictionary<string, object>? AdditionalProperties;
        public readonly Outputs.InputsPropertiesAdditionalPropertiesChoice1Of5? Choice1Of5;
        public readonly Outputs.Choice2Of5? Choice2Of5;
        public readonly Outputs.PropertiesAdditionalPropertiesChoice3Of5? Choice3Of5;
        public readonly Outputs.NamedType? Choice4Of5;
        public readonly Outputs.PropertiesAdditionalPropertiesChoice5Of5? Choice5Of5;
        public readonly object? Const;
        public readonly object? Default;
        public readonly Outputs.DefaultInfo? DefaultInfo;
        /// <summary>
        /// Indicates whether the property is deprecated
        /// </summary>
        public readonly string? DeprecationMessage;
        /// <summary>
        /// The description of the property, if any. Interpreted as Markdown.
        /// </summary>
        public readonly string? Description;
        /// <summary>
        /// Additional language-specific data about the property.
        /// </summary>
        public readonly ImmutableDictionary<string, object>? Language;
        /// <summary>
        /// Indicates that when used as an input, this type does not accept eventual values.
        /// </summary>
        public readonly bool? Plain;
        /// <summary>
        /// Specifies whether a change to the property causes its containing resource to be replaced instead of updated (default false).
        /// </summary>
        public readonly bool? ReplaceOnChanges;
        /// <summary>
        /// Specifies whether the property is secret (default false).
        /// </summary>
        public readonly bool? Secret;
        /// <summary>
        /// Indicates that the provider will replace the resource when this property is changed.
        /// </summary>
        public readonly bool? WillReplaceOnChanges;

        [OutputConstructor]
        private AdditionalProperties(
            ImmutableDictionary<string, object>? additionalProperties,

            Outputs.InputsPropertiesAdditionalPropertiesChoice1Of5? choice1Of5,

            Outputs.Choice2Of5? choice2Of5,

            Outputs.PropertiesAdditionalPropertiesChoice3Of5? choice3Of5,

            Outputs.NamedType? choice4Of5,

            Outputs.PropertiesAdditionalPropertiesChoice5Of5? choice5Of5,

            object? @const,

            object? @default,

            Outputs.DefaultInfo? defaultInfo,

            string? deprecationMessage,

            string? description,

            ImmutableDictionary<string, object>? language,

            bool? plain,

            bool? replaceOnChanges,

            bool? secret,

            bool? willReplaceOnChanges)
        {
            AdditionalProperties = additionalProperties;
            Choice1Of5 = choice1Of5;
            Choice2Of5 = choice2Of5;
            Choice3Of5 = choice3Of5;
            Choice4Of5 = choice4Of5;
            Choice5Of5 = choice5Of5;
            Const = @const;
            Default = @default;
            DefaultInfo = defaultInfo;
            DeprecationMessage = deprecationMessage;
            Description = description;
            Language = language;
            Plain = plain;
            ReplaceOnChanges = replaceOnChanges;
            Secret = secret;
            WillReplaceOnChanges = willReplaceOnChanges;
        }
    }
}
