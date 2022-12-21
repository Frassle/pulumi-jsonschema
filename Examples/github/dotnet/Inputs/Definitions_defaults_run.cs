// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.GithubWorkflow.Inputs
{

    public sealed class Definitions_defaults_run : global::Pulumi.InvokeArgs
    {
        /// <summary>
        /// unhandled schema: Json.Schema.CommentKeyword, Json.Schema.DescriptionKeyword, Json.Schema.AnyOfKeyword
        /// </summary>
        [Input("shell")]
        public object? Shell { get; set; }

        /// <summary>
        /// Using the working-directory keyword, you can specify the working directory of where to run the command.
        /// </summary>
        [Input("working-directory")]
        public string? Working-directory { get; set; }

        public Definitions_defaults_run()
        {
        }
        public static new Definitions_defaults_run Empty => new Definitions_defaults_run();
    }
}