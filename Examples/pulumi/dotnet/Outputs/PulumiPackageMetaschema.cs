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
    /// A description of the schema for a Pulumi Package
    /// </summary>
    [OutputType]
    public sealed class PulumiPackageMetaschema
    {
        /// <summary>
        /// Freeform text attribution of derived work, if required.
        /// </summary>
        public readonly string? Attribution;
        public readonly Outputs.Config? Config;
        /// <summary>
        /// The description of the package. Descriptions are interpreted as Markdown.
        /// </summary>
        public readonly string? Description;
        /// <summary>
        /// The human-friendly name of the package.
        /// </summary>
        public readonly string? DisplayName;
        /// <summary>
        /// A map from token to functionSpec that describes the set of functions defined by this package.
        /// </summary>
        public readonly ImmutableDictionary<string, Outputs.FunctionDefinition>? Functions;
        /// <summary>
        /// The package's homepage.
        /// </summary>
        public readonly string? Homepage;
        /// <summary>
        /// The list of keywords that are associated with the package, if any.
        /// </summary>
        public readonly ImmutableArray<string> Keywords;
        /// <summary>
        /// Additional language-specific data about the package.
        /// </summary>
        public readonly ImmutableDictionary<string, object>? Language;
        /// <summary>
        /// The name of the license used for the package's contents.
        /// </summary>
        public readonly string? License;
        /// <summary>
        /// The URL of the package's logo, if any.
        /// </summary>
        public readonly string? LogoUrl;
        public readonly Outputs.Meta? Meta;
        /// <summary>
        /// The unqualified name of the package (e.g. "aws", "azure", "gcp", "kubernetes", "random")
        /// </summary>
        public readonly string Name;
        /// <summary>
        /// The URL to use when downloading the provider plugin binary.
        /// </summary>
        public readonly string? PluginDownloadURL;
        public readonly Outputs.ResourceDefinition? Provider;
        /// <summary>
        /// The name of the person or organization that authored and published the package.
        /// </summary>
        public readonly string? Publisher;
        /// <summary>
        /// The URL at which the package's sources can be found.
        /// </summary>
        public readonly string? Repository;
        /// <summary>
        /// A map from type token to resourceSpec that describes the set of resources and components defined by this package.
        /// </summary>
        public readonly ImmutableDictionary<string, Outputs.ResourceDefinition>? Resources;
        /// <summary>
        /// A map from type token to complexTypeSpec that describes the set of complex types (i.e. object, enum) defined by this package.
        /// </summary>
        public readonly ImmutableDictionary<string, object>? Types;
        /// <summary>
        /// The version of the package. The version must be valid semver.
        /// </summary>
        public readonly string? Version;

        [OutputConstructor]
        private PulumiPackageMetaschema(
            string? attribution,

            Outputs.Config? config,

            string? description,

            string? displayName,

            ImmutableDictionary<string, Outputs.FunctionDefinition>? functions,

            string? homepage,

            ImmutableArray<string> keywords,

            ImmutableDictionary<string, object>? language,

            string? license,

            string? logoUrl,

            Outputs.Meta? meta,

            string name,

            string? pluginDownloadURL,

            Outputs.ResourceDefinition? provider,

            string? publisher,

            string? repository,

            ImmutableDictionary<string, Outputs.ResourceDefinition>? resources,

            ImmutableDictionary<string, object>? types,

            string? version)
        {
            Attribution = attribution;
            Config = config;
            Description = description;
            DisplayName = displayName;
            Functions = functions;
            Homepage = homepage;
            Keywords = keywords;
            Language = language;
            License = license;
            LogoUrl = logoUrl;
            Meta = meta;
            Name = name;
            PluginDownloadURL = pluginDownloadURL;
            Provider = provider;
            Publisher = publisher;
            Repository = repository;
            Resources = resources;
            Types = types;
            Version = version;
        }
    }
}
