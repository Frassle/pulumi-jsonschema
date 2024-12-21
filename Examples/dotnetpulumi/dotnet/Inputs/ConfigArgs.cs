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
    public sealed class ConfigArgs : global::Pulumi.ResourceArgs
    {
        [Input("defaults")]
        private InputList<string>? _defaults;

        /// <summary>
        /// A list of the names of the package's non-required configuration variables.
        /// </summary>
        public InputList<string> Defaults
        {
            get => _defaults ?? (_defaults = new InputList<string>());
            set => _defaults = value;
        }

        [Input("variables")]
        private InputMap<Inputs.VariablesAdditionalPropertiesArgs>? _variables;

        /// <summary>
        /// A map from variable name to propertySpec that describes a package's configuration variables.
        /// </summary>
        public InputMap<Inputs.VariablesAdditionalPropertiesArgs> Variables
        {
            get => _variables ?? (_variables = new InputMap<Inputs.VariablesAdditionalPropertiesArgs>());
            set => _variables = value;
        }

        public ConfigArgs()
        {
        }
        public static new ConfigArgs Empty => new ConfigArgs();
    }
}
