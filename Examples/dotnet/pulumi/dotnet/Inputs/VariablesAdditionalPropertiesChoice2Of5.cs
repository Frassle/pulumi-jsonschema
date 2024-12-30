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
    /// A reference to an array type. The "type" property must be set to "array" and the "items" property must be present. No other properties may be present.
    /// </summary>
    public sealed class VariablesAdditionalPropertiesChoice2Of5 : global::Pulumi.InvokeArgs
    {
        /// <summary>
        /// Describes an object or resource property
        /// </summary>
        [Input("items", required: true)]
        public Inputs.VariablesAdditionalProperties Items { get; set; } = null!;

        [Input("type", required: true)]
        public string Type { get; set; } = null!;

        public VariablesAdditionalPropertiesChoice2Of5()
        {
        }
        public static new VariablesAdditionalPropertiesChoice2Of5 Empty => new VariablesAdditionalPropertiesChoice2Of5();
    }
}
