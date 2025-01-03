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
    /// Options for assets paths resolution and how to build assets collection
    /// </summary>
    public sealed class AssetsConfiguration : global::Pulumi.InvokeArgs
    {
        /// <summary>
        /// Allow build assets for dependencies.
        /// </summary>
        [Input("dependencies")]
        public bool? Dependencies { get; set; }

        [Input("followSymlinks")]
        public bool? FollowSymlinks { get; set; }

        [Input("method")]
        public Pulumi.Cargo.Method? Method { get; set; }

        /// <summary>
        /// Allow overwriting existing files.
        /// </summary>
        [Input("overwrite")]
        public bool? Overwrite { get; set; }

        public AssetsConfiguration()
        {
        }
        public static new AssetsConfiguration Empty => new AssetsConfiguration();
    }
}
