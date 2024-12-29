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
    /// Profiles provide a way to alter the compiler settings, influencing things like optimizations and debugging symbols.
    /// 
    /// Cargo has 4 built-in profiles: dev, release, test, and bench. It automatically chooses the profile based on which command is being run, the package and target that is being built, and command-line flags like --release.
    /// </summary>
    public sealed class Profiles : global::Pulumi.InvokeArgs
    {
        [Input("additionalProperties")]
        private Dictionary<string, Inputs.Profile>? _additionalProperties;
        public Dictionary<string, Inputs.Profile> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new Dictionary<string, Inputs.Profile>());
            set => _additionalProperties = value;
        }

        [Input("bench")]
        public Inputs.DefinitionsProfile? Bench { get; set; }

        [Input("dev")]
        public Inputs.Dev? Dev { get; set; }

        [Input("release")]
        public Inputs.Release? Release { get; set; }

        [Input("test")]
        public Inputs.Test? Test { get; set; }

        public Profiles()
        {
        }
        public static new Profiles Empty => new Profiles();
    }
}
