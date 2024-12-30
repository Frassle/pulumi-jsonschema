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
    /// A description of the schema for a Pulumi Package
    /// </summary>
    public sealed class PulumiPackageMetaschemaArgs : global::Pulumi.ResourceArgs
    {
        /// <summary>
        /// Freeform text attribution of derived work, if required.
        /// </summary>
        [Input("attribution")]
        public Input<string>? Attribution { get; set; }

        /// <summary>
        /// The package's configuration variables.
        /// </summary>
        [Input("config")]
        public Input<Inputs.ConfigArgs>? Config { get; set; }

        /// <summary>
        /// The description of the package. Descriptions are interpreted as Markdown.
        /// </summary>
        [Input("description")]
        public Input<string>? Description { get; set; }

        /// <summary>
        /// The human-friendly name of the package.
        /// </summary>
        [Input("displayName")]
        public Input<string>? DisplayName { get; set; }

        [Input("functions")]
        private InputMap<Inputs.FunctionDefinitionArgs>? _functions;

        /// <summary>
        /// A map from token to functionSpec that describes the set of functions defined by this package.
        /// </summary>
        public InputMap<Inputs.FunctionDefinitionArgs> Functions
        {
            get => _functions ?? (_functions = new InputMap<Inputs.FunctionDefinitionArgs>());
            set => _functions = value;
        }

        /// <summary>
        /// The package's homepage.
        /// </summary>
        [Input("homepage")]
        public Input<string>? Homepage { get; set; }

        [Input("keywords")]
        private InputList<string>? _keywords;

        /// <summary>
        /// The list of keywords that are associated with the package, if any.
        /// </summary>
        public InputList<string> Keywords
        {
            get => _keywords ?? (_keywords = new InputList<string>());
            set => _keywords = value;
        }

        [Input("language")]
        private InputMap<object>? _language;

        /// <summary>
        /// Additional language-specific data about the package.
        /// </summary>
        public InputMap<object> Language
        {
            get => _language ?? (_language = new InputMap<object>());
            set => _language = value;
        }

        /// <summary>
        /// The name of the license used for the package's contents.
        /// </summary>
        [Input("license")]
        public Input<string>? License { get; set; }

        /// <summary>
        /// The URL of the package's logo, if any.
        /// </summary>
        [Input("logoUrl")]
        public Input<string>? LogoUrl { get; set; }

        /// <summary>
        /// Format metadata about this package.
        /// </summary>
        [Input("meta")]
        public Input<Inputs.MetaArgs>? Meta { get; set; }

        /// <summary>
        /// The unqualified name of the package (e.g. "aws", "azure", "gcp", "kubernetes", "random")
        /// </summary>
        [Input("name", required: true)]
        public Input<string> Name { get; set; } = null!;

        /// <summary>
        /// An optional object to define parameterization for the package.
        /// </summary>
        [Input("parameterization")]
        public Input<Inputs.ParameterizationArgs>? Parameterization { get; set; }

        /// <summary>
        /// The URL to use when downloading the provider plugin binary.
        /// </summary>
        [Input("pluginDownloadURL")]
        public Input<string>? PluginDownloadURL { get; set; }

        /// <summary>
        /// Describes a resource or component.
        /// </summary>
        [Input("provider")]
        public Input<Inputs.ObjectTypeSpecArgs>? Provider { get; set; }

        /// <summary>
        /// The name of the person or organization that authored and published the package.
        /// </summary>
        [Input("publisher")]
        public Input<string>? Publisher { get; set; }

        /// <summary>
        /// The URL at which the package's sources can be found.
        /// </summary>
        [Input("repository")]
        public Input<string>? Repository { get; set; }

        [Input("resources")]
        private InputMap<Inputs.ResourceDefinitionArgs>? _resources;

        /// <summary>
        /// A map from type token to resourceSpec that describes the set of resources and components defined by this package.
        /// </summary>
        public InputMap<Inputs.ResourceDefinitionArgs> Resources
        {
            get => _resources ?? (_resources = new InputMap<Inputs.ResourceDefinitionArgs>());
            set => _resources = value;
        }

        [Input("types")]
        private InputMap<Inputs.TypeDefinitionArgs>? _types;

        /// <summary>
        /// A map from type token to complexTypeSpec that describes the set of complex types (i.e. object, enum) defined by this package.
        /// </summary>
        public InputMap<Inputs.TypeDefinitionArgs> Types
        {
            get => _types ?? (_types = new InputMap<Inputs.TypeDefinitionArgs>());
            set => _types = value;
        }

        /// <summary>
        /// The version of the package. The version must be valid semver.
        /// </summary>
        [Input("version")]
        public Input<string>? Version { get; set; }

        public PulumiPackageMetaschemaArgs()
        {
        }
        public static new PulumiPackageMetaschemaArgs Empty => new PulumiPackageMetaschemaArgs();
    }
}
