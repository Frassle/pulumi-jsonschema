// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.GithubWorkflow.Inputs
{

    public sealed class Definitions_defaults : global::Pulumi.InvokeArgs
    {
        [Input("run")]
        public Inputs.Definitions_defaults_run? Run { get; set; }

        public Definitions_defaults()
        {
        }
        public static new Definitions_defaults Empty => new Definitions_defaults();
    }
}