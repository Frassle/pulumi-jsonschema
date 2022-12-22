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
    /// Describes a function.
    /// </summary>
    [OutputType]
    public sealed class FunctionDefinition
    {
        public readonly ImmutableDictionary<string, object>? AdditionalProperties;
        /// <summary>
        /// Indicates whether the function is deprecated
        /// </summary>
        public readonly string? DeprecationMessage;
        /// <summary>
        /// The description of the function, if any. Interpreted as Markdown.
        /// </summary>
        public readonly string? Description;
        public readonly Outputs.ObjectTypeDetails0? Inputs;
        /// <summary>
        /// Indicates that the implementation of the function should not be generated from the schema, and is instead provided out-of-band by the package author
        /// </summary>
        public readonly bool? IsOverlay;
        /// <summary>
        /// Additional language-specific data about the function.
        /// </summary>
        public readonly ImmutableDictionary<string, object>? Language;
        public readonly Outputs.ObjectTypeDetails? Outputs;

        [OutputConstructor]
        private FunctionDefinition(
            ImmutableDictionary<string, object>? additionalProperties,

            string? deprecationMessage,

            string? description,

            Outputs.ObjectTypeDetails0? inputs,

            bool? isOverlay,

            ImmutableDictionary<string, object>? language,

            Outputs.ObjectTypeDetails? outputs)
        {
            AdditionalProperties = additionalProperties;
            DeprecationMessage = deprecationMessage;
            Description = description;
            Inputs = inputs;
            IsOverlay = isOverlay;
            Language = language;
            Outputs = outputs;
        }
    }
}
