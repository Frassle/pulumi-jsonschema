// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.GithubWorkflow.Inputs
{

    public sealed class DefinitionsDefaultsRun : global::Pulumi.InvokeArgs
    {
        /// <summary>
        /// unhandled schema: Json.Schema.CommentKeyword, Json.Schema.DescriptionKeyword, Json.Schema.AnyOfKeyword
        /// </summary>
        [Input("shell")]
        public object? Shell { get; set; }

        /// <summary>
        /// Using the working-directory keyword, you can specify the working directory of where to run the command.
        /// </summary>
        [Input("workingDirectory")]
        public string? WorkingDirectory { get; set; }

        public DefinitionsDefaultsRun()
        {
        }
        public static new DefinitionsDefaultsRun Empty => new DefinitionsDefaultsRun();
    }
}