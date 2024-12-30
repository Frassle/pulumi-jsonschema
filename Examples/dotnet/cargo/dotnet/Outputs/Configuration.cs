// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.Cargo.Outputs
{

    /// <summary>
    /// Package build options.
    /// </summary>
    [OutputType]
    public sealed class Configuration
    {
        public readonly ImmutableDictionary<string, object>? AdditionalProperties;
        public readonly Outputs.AssetsConfiguration? Assets;

        [OutputConstructor]
        private Configuration(
            ImmutableDictionary<string, object>? additionalProperties,

            Outputs.AssetsConfiguration? assets)
        {
            AdditionalProperties = additionalProperties;
            Assets = assets;
        }
    }
}