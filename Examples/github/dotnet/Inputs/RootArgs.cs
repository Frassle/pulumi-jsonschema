// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.GithubWorkflow.Inputs
{

    public sealed class RootArgs : global::Pulumi.ResourceArgs
    {
        /// <summary>
        /// default any for anyOf
        /// </summary>
        [Input("concurrency")]
        public Input<object>? Concurrency { get; set; }

        [Input("defaults")]
        public Input<Inputs.DefinitionsDefaultsArgs>? Defaults { get; set; }

        /// <summary>
        /// default any for anyOf
        /// </summary>
        [Input("env")]
        public Input<object>? Env { get; set; }

        [Input("jobs", required: true)]
        public Input<Inputs.JobsArgs> Jobs { get; set; } = null!;

        /// <summary>
        /// The name of your workflow. GitHub displays the names of your workflows on your repository's actions page. If you omit this field, GitHub sets the name to the workflow's filename.
        /// </summary>
        [Input("name")]
        public Input<string>? Name { get; set; }

        /// <summary>
        /// default any for anyOf
        /// </summary>
        [Input("on", required: true)]
        public Input<object> On { get; set; } = null!;

        /// <summary>
        /// default any for anyOf
        /// </summary>
        [Input("permissions")]
        public Input<object>? Permissions { get; set; }

        /// <summary>
        /// The name for workflow runs generated from the workflow. GitHub displays the workflow run name in the list of workflow runs on your repository's 'Actions' tab.
        /// </summary>
        [Input("runName")]
        public Input<string>? RunName { get; set; }

        public RootArgs()
        {
        }
        public static new RootArgs Empty => new RootArgs();
    }
}
