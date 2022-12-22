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
    /// Additional information about the property's default value, if any.
    /// </summary>
    [OutputType]
    public sealed class PropertySpecDefaultInfo0
    {
        public readonly ImmutableDictionary<string, object>? AdditionalProperties;
        /// <summary>
        /// A set of environment variables to probe for a default value.
        /// </summary>
        public readonly ImmutableArray<string> Environment;
        /// <summary>
        /// Additional language-specific data about the default value.
        /// </summary>
        public readonly ImmutableDictionary<string, object>? Language;

        [OutputConstructor]
        private PropertySpecDefaultInfo0(
            ImmutableDictionary<string, object>? additionalProperties,

            ImmutableArray<string> environment,

            ImmutableDictionary<string, object>? language)
        {
            AdditionalProperties = additionalProperties;
            Environment = environment;
            Language = language;
        }
    }
}
