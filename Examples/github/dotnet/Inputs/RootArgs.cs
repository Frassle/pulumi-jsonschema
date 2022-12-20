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
        [Input("concurrency")]
        public Input<object>? Concurrency { get; set; }

        [Input("defaults")]
        private InputMap<string>? _defaults;
        public InputMap<string> Defaults
        {
            get => _defaults ?? (_defaults = new InputMap<string>());
            set => _defaults = value;
        }

        [Input("env")]
        public Input<object>? Env { get; set; }

        [Input("jobs", required: true)]
        private InputMap<string>? _jobs;

        /// <summary>
        /// A workflow run is made up of one or more jobs. Jobs run in parallel by default. To run jobs sequentially, you can define dependencies on other jobs using the jobs.&lt;job_id&gt;.needs keyword.
        /// Each job runs in a fresh instance of the virtual environment specified by runs-on.
        /// You can run an unlimited number of jobs as long as you are within the workflow usage limits. For more information, see https://help.github.com/en/github/automating-your-workflow-with-github-actions/workflow-syntax-for-github-actions#usage-limits.
        /// </summary>
        public InputMap<string> Jobs
        {
            get => _jobs ?? (_jobs = new InputMap<string>());
            set => _jobs = value;
        }

        /// <summary>
        /// The name of your workflow. GitHub displays the names of your workflows on your repository's actions page. If you omit this field, GitHub sets the name to the workflow's filename.
        /// </summary>
        [Input("name")]
        public Input<string>? Name { get; set; }

        [Input("on", required: true)]
        public Input<object> On { get; set; } = null!;

        [Input("permissions")]
        public Input<object>? Permissions { get; set; }

        /// <summary>
        /// The name for workflow runs generated from the workflow. GitHub displays the workflow run name in the list of workflow runs on your repository's 'Actions' tab.
        /// </summary>
        [Input("run-name")]
        public Input<string>? Run-name { get; set; }

        public RootArgs()
        {
        }
        public static new RootArgs Empty => new RootArgs();
    }
}