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
    public sealed class PropertiesAdditionalPropertiesChoice1Of5Args : global::Pulumi.ResourceArgs
    {
        [Input("type", required: true)]
        public Input<Pulumi.Pulumi.TypeSpecOneOf0PropertiesType> Type { get; set; } = null!;

        public PropertiesAdditionalPropertiesChoice1Of5Args()
        {
        }
        public static new PropertiesAdditionalPropertiesChoice1Of5Args Empty => new PropertiesAdditionalPropertiesChoice1Of5Args();
    }
}
