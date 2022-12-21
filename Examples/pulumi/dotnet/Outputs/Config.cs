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
    /// The package's configuration variables.
    /// </summary>
    [OutputType]
    public sealed class Config
    {
        /// <summary>
        /// A list of the names of the package's non-required configuration variables.
        /// </summary>
        public readonly ImmutableArray<string> Defaults;
        /// <summary>
        /// A map from variable name to propertySpec that describes a package's configuration variables.
        /// </summary>
        public readonly ImmutableDictionary<string, object>? Variables;

        [OutputConstructor]
        private Config(
            ImmutableArray<string> defaults,

            ImmutableDictionary<string, object>? variables)
        {
            Defaults = defaults;
            Variables = variables;
        }
    }
}
