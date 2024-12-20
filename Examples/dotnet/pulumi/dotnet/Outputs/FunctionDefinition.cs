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
        public readonly Outputs.ObjectTypeDetails? Inputs;
        /// <summary>
        /// Indicates that the implementation of the function should not be generated from the schema, and is instead provided out-of-band by the package author
        /// </summary>
        public readonly bool? IsOverlay;
        /// <summary>
        /// Additional language-specific data about the function.
        /// </summary>
        public readonly ImmutableDictionary<string, object>? Language;
        /// <summary>
        /// A list of parameter names that determines whether the input bag should be treated as a single argument or as multiple arguments. The list corresponds to the order in which the parameters should be passed to the function.
        /// </summary>
        public readonly ImmutableArray<string> MultiArgumentInputs;
        /// <summary>
        /// unhandled schema: Json.Schema.DescriptionKeyword, Json.Schema.AnyOfKeyword
        /// </summary>
        public readonly object? Outputs;

        [OutputConstructor]
        private FunctionDefinition(
            ImmutableDictionary<string, object>? additionalProperties,

            string? deprecationMessage,

            string? description,

            Outputs.ObjectTypeDetails? inputs,

            bool? isOverlay,

            ImmutableDictionary<string, object>? language,

            ImmutableArray<string> multiArgumentInputs,

            object? outputs)
        {
            AdditionalProperties = additionalProperties;
            DeprecationMessage = deprecationMessage;
            Description = description;
            Inputs = inputs;
            IsOverlay = isOverlay;
            Language = language;
            MultiArgumentInputs = multiArgumentInputs;
            Outputs = outputs;
        }
    }
}