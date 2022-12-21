// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.GithubWorkflow.Inputs
{

    public sealed class Root : global::Pulumi.InvokeArgs
    {
        /// <summary>
        /// default any for anyOf
        /// </summary>
        [Input("concurrency")]
        public object? Concurrency { get; set; }

        [Input("defaults")]
        public Inputs.DefinitionsDefaults? Defaults { get; set; }

        /// <summary>
        /// default any for anyOf
        /// </summary>
        [Input("env")]
        public object? Env { get; set; }

        [Input("jobs", required: true)]
        public Inputs.Jobs Jobs { get; set; } = null!;

        /// <summary>
        /// The name of your workflow. GitHub displays the names of your workflows on your repository's actions page. If you omit this field, GitHub sets the name to the workflow's filename.
        /// </summary>
        [Input("name")]
        public string? Name { get; set; }

        /// <summary>
        /// default any for anyOf
        /// </summary>
        [Input("on", required: true)]
        public object On { get; set; } = null!;

        /// <summary>
        /// default any for anyOf
        /// </summary>
        [Input("permissions")]
        public object? Permissions { get; set; }

        /// <summary>
        /// The name for workflow runs generated from the workflow. GitHub displays the workflow run name in the list of workflow runs on your repository's 'Actions' tab.
        /// </summary>
        [Input("runName")]
        public string? RunName { get; set; }

        public Root()
        {
        }
        public static new Root Empty => new Root();
    }
}
