// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.Jsonschema.Inputs
{

    public sealed class Root : global::Pulumi.InvokeArgs
    {
        [Input("attribution")]
        public string? Attribution { get; set; }

        [Input("config")]
        private Dictionary<string, string>? _config;
        public Dictionary<string, string> Config
        {
            get => _config ?? (_config = new Dictionary<string, string>());
            set => _config = value;
        }

        [Input("description")]
        public string? Description { get; set; }

        [Input("displayName")]
        public string? DisplayName { get; set; }

        [Input("functions")]
        private Dictionary<string, ImmutableDictionary<string, string>>? _functions;
        public Dictionary<string, ImmutableDictionary<string, string>> Functions
        {
            get => _functions ?? (_functions = new Dictionary<string, ImmutableDictionary<string, string>>());
            set => _functions = value;
        }

        [Input("homepage")]
        public string? Homepage { get; set; }

        [Input("keywords")]
        private List<string>? _keywords;
        public List<string> Keywords
        {
            get => _keywords ?? (_keywords = new List<string>());
            set => _keywords = value;
        }

        [Input("language")]
        private Dictionary<string, object>? _language;
        public Dictionary<string, object> Language
        {
            get => _language ?? (_language = new Dictionary<string, object>());
            set => _language = value;
        }

        [Input("license")]
        public string? License { get; set; }

        [Input("logoUrl")]
        public string? LogoUrl { get; set; }

        [Input("meta")]
        private Dictionary<string, string>? _meta;
        public Dictionary<string, string> Meta
        {
            get => _meta ?? (_meta = new Dictionary<string, string>());
            set => _meta = value;
        }

        [Input("name", required: true)]
        public string Name { get; set; } = null!;

        [Input("pluginDownloadURL")]
        public string? PluginDownloadURL { get; set; }

        [Input("provider")]
        public object? Provider { get; set; }

        [Input("publisher")]
        public string? Publisher { get; set; }

        [Input("repository")]
        public string? Repository { get; set; }

        [Input("resources")]
        private Dictionary<string, object>? _resources;
        public Dictionary<string, object> Resources
        {
            get => _resources ?? (_resources = new Dictionary<string, object>());
            set => _resources = value;
        }

        [Input("types")]
        private Dictionary<string, object>? _types;
        public Dictionary<string, object> Types
        {
            get => _types ?? (_types = new Dictionary<string, object>());
            set => _types = value;
        }

        [Input("version")]
        public string? Version { get; set; }

        public Root()
        {
        }
        public static new Root Empty => new Root();
    }
}
