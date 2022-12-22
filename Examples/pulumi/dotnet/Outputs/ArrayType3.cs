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
    /// A reference to an array type. The "type" property must be set to "array" and the "items" property must be present. No other properties may be present.
    /// </summary>
    [OutputType]
    public sealed class ArrayType3
    {
        public readonly Outputs.PropertyDefinition4 Items;
        public readonly string Type;

        [OutputConstructor]
        private ArrayType3(
            Outputs.PropertyDefinition4 items,

            string type)
        {
            Items = items;
            Type = type;
        }
    }
}
