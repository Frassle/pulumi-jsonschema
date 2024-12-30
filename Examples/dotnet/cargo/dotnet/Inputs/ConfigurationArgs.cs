// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.Cargo.Inputs
{

    /// <summary>
    /// Package build options.
    /// </summary>
    public sealed class ConfigurationArgs : global::Pulumi.ResourceArgs
    {
        [Input("additionalProperties")]
        private InputMap<object>? _additionalProperties;
        public InputMap<object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new InputMap<object>());
            set => _additionalProperties = value;
        }

        /// <summary>
        /// Options for assets paths resolution and how to build assets collection
        /// </summary>
        [Input("assets")]
        public Input<Inputs.AssetsConfigurationArgs>? Assets { get; set; }

        public ConfigurationArgs()
        {
        }
        public static new ConfigurationArgs Empty => new ConfigurationArgs();
    }
}
