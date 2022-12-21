// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.Pulumi.Outputs
{

    [OutputType]
    public sealed class AliasDefinition
    {
        public readonly ImmutableDictionary<string, object>? AdditionalProperties;
        /// <summary>
        /// The name portion of the alias, if any
        /// </summary>
        public readonly string? Name;
        /// <summary>
        /// The project portion of the alias, if any
        /// </summary>
        public readonly string? Project;
        /// <summary>
        /// The type portion of the alias, if any
        /// </summary>
        public readonly string? Type;

        [OutputConstructor]
        private AliasDefinition(
            ImmutableDictionary<string, object>? additionalProperties,

            string? name,

            string? project,

            string? type)
        {
            AdditionalProperties = additionalProperties;
            Name = name;
            Project = project;
            Type = type;
        }
    }
}
