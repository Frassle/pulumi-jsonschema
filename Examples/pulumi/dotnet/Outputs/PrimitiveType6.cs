// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.Pulumi.Outputs
{

    /// <summary>
    /// A reference to a primitive type. A primitive type must have only the "type" property set.
    /// </summary>
    [OutputType]
    public sealed class PrimitiveType6
    {
        public readonly Pulumi.Pulumi.ConfigVariablesAdditionalPropertiesChoice1Of5Type Type;

        [OutputConstructor]
        private PrimitiveType6(Pulumi.Pulumi.ConfigVariablesAdditionalPropertiesChoice1Of5Type type)
        {
            Type = type;
        }
    }
}
