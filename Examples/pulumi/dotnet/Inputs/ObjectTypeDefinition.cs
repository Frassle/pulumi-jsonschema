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
    /// Describes an object type
    /// </summary>
    public sealed class ObjectTypeDefinition : global::Pulumi.InvokeArgs
    {
        [Input("properties")]
        private Dictionary<string, Inputs.AdditionalProperties>? _properties;

        /// <summary>
        /// A map from property name to propertySpec that describes the object's properties.
        /// </summary>
        public Dictionary<string, Inputs.AdditionalProperties> Properties
        {
            get => _properties ?? (_properties = new Dictionary<string, Inputs.AdditionalProperties>());
            set => _properties = value;
        }

        [Input("required")]
        private List<string>? _required;

        /// <summary>
        /// A list of the names of an object type's required properties. These properties must be set for inputs and will always be set for outputs.
        /// </summary>
        public List<string> Required
        {
            get => _required ?? (_required = new List<string>());
            set => _required = value;
        }

        [Input("type")]
        public string? Type { get; set; }

        public ObjectTypeDefinition()
        {
        }
        public static new ObjectTypeDefinition Empty => new ObjectTypeDefinition();
    }
}
