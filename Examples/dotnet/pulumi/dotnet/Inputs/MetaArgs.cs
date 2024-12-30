// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.Pulumi.Inputs
{

    /// <summary>
    /// Format metadata about this package.
    /// </summary>
    public sealed class MetaArgs : global::Pulumi.ResourceArgs
    {
        /// <summary>
        /// A regex that is used by the importer to extract a module name from the module portion of a type token. Packages that use the module format "namespace1/namespace2/.../namespaceN" do not need to specify a format. The regex must define one capturing group that contains the module name, which must be formatted as "namespace1/namespace2/...namespaceN".
        /// </summary>
        [Input("moduleFormat")]
        public string? ModuleFormat { get; set; }

        /// <summary>
        /// Write the package to support the pack command.
        /// </summary>
        [Input("supportPack")]
        public bool? SupportPack { get; set; }

        public MetaArgs()
        {
        }
        public static new MetaArgs Empty => new MetaArgs();
    }
}
