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
    /// A reference to a primitive type. A primitive type must have only the "type" property set.
    /// </summary>
    public sealed class AdditionalPropertiesChoice1Of5 : global::Pulumi.InvokeArgs
    {
        [Input("type", required: true)]
        public Pulumi.Pulumi.InputPropertiesAdditionalPropertiesChoice1Of5Type Type { get; set; }

        public AdditionalPropertiesChoice1Of5()
        {
        }
        public static new AdditionalPropertiesChoice1Of5 Empty => new AdditionalPropertiesChoice1Of5();
    }
}
