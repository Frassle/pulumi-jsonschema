// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.GithubWorkflow.Inputs
{

    public sealed class DefinitionsDefaults : global::Pulumi.InvokeArgs
    {
        [Input("run")]
        public Inputs.DefinitionsDefaultsRun? Run { get; set; }

        public DefinitionsDefaults()
        {
        }
        public static new DefinitionsDefaults Empty => new DefinitionsDefaults();
    }
}