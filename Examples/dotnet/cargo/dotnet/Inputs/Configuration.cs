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
    public sealed class Configuration : global::Pulumi.InvokeArgs
    {
        [Input("additionalProperties")]
        private Dictionary<string, object>? _additionalProperties;
        public Dictionary<string, object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new Dictionary<string, object>());
            set => _additionalProperties = value;
        }

        /// <summary>
        /// Options for assets paths resolution and how to build assets collection
        /// </summary>
        [Input("assets")]
        public Inputs.AssetsConfiguration? Assets { get; set; }

        public Configuration()
        {
        }
        public static new Configuration Empty => new Configuration();
    }
}
