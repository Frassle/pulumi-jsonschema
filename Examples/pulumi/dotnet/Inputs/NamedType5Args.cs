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
    /// A reference to a type in this or another document. The "$ref" property must be present. The "type" property is ignored if it is present. No other properties may be present.
    /// </summary>
    public sealed class NamedType5Args : global::Pulumi.ResourceArgs
    {
        /// <summary>
        /// The URI of the referenced type. For example, the built-in Archive, Asset, and Any
        /// types are referenced as "pulumi.json#/Archive", "pulumi.json#/Asset", and "pulumi.json#/Any", respectively.
        /// A type from this document is referenced as "#/types/pulumi:type:token".
        /// A type from another document is referenced as "path#/types/pulumi:type:token", where path is of the form:
        ///   "/provider/vX.Y.Z/schema.json" or "pulumi.json" or "http[s]://example.com/provider/vX.Y.Z/schema.json"
        /// A resource from this document is referenced as "#/resources/pulumi:type:token".
        /// A resource from another document is referenced as "path#/resources/pulumi:type:token", where path is of the form:
        ///   "/provider/vX.Y.Z/schema.json" or "pulumi.json" or "http[s]://example.com/provider/vX.Y.Z/schema.json"
        /// </summary>
        [Input("$ref", required: true)]
        public Input<string> $ref { get; set; } = null!;

        /// <summary>
        /// ignored; present for compatibility with existing schemas
        /// </summary>
        [Input("type")]
        public Input<string>? Type { get; set; }

        public NamedType5Args()
        {
        }
        public static new NamedType5Args Empty => new NamedType5Args();
    }
}
