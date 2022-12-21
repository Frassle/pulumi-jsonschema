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
    /// Describes a function.
    /// </summary>
    public sealed class FunctionDefinition : global::Pulumi.InvokeArgs
    {
        [Input("additionalProperties")]
        private Dictionary<string, object>? _additionalProperties;
        public Dictionary<string, object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new Dictionary<string, object>());
            set => _additionalProperties = value;
        }

        /// <summary>
        /// Indicates whether the function is deprecated
        /// </summary>
        [Input("deprecationMessage")]
        public string? DeprecationMessage { get; set; }

        /// <summary>
        /// The description of the function, if any. Interpreted as Markdown.
        /// </summary>
        [Input("description")]
        public string? Description { get; set; }

        [Input("inputs")]
        public Inputs.ObjectTypeDetails? Inputs { get; set; }

        /// <summary>
        /// Indicates that the implementation of the function should not be generated from the schema, and is instead provided out-of-band by the package author
        /// </summary>
        [Input("isOverlay")]
        public bool? IsOverlay { get; set; }

        [Input("language")]
        private Dictionary<string, object>? _language;

        /// <summary>
        /// Additional language-specific data about the function.
        /// </summary>
        public Dictionary<string, object> Language
        {
            get => _language ?? (_language = new Dictionary<string, object>());
            set => _language = value;
        }

        [Input("outputs")]
        public Inputs.ObjectTypeDetails? Outputs { get; set; }

        public FunctionDefinition()
        {
        }
        public static new FunctionDefinition Empty => new FunctionDefinition();
    }
}