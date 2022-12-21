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
    /// The package's configuration variables.
    /// </summary>
    public sealed class Config : global::Pulumi.InvokeArgs
    {
        [Input("defaults")]
        private List<string>? _defaults;

        /// <summary>
        /// A list of the names of the package's non-required configuration variables.
        /// </summary>
        public List<string> Defaults
        {
            get => _defaults ?? (_defaults = new List<string>());
            set => _defaults = value;
        }

        [Input("variables")]
        private Dictionary<string, object>? _variables;

        /// <summary>
        /// A map from variable name to propertySpec that describes a package's configuration variables.
        /// </summary>
        public Dictionary<string, object> Variables
        {
            get => _variables ?? (_variables = new Dictionary<string, object>());
            set => _variables = value;
        }

        public Config()
        {
        }
        public static new Config Empty => new Config();
    }
}
