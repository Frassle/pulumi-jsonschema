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
    public sealed class ObjectTypeDetails0 : global::Pulumi.InvokeArgs
    {
        [Input("additionalProperties")]
        private Dictionary<string, object>? _additionalProperties;
        public Dictionary<string, object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new Dictionary<string, object>());
            set => _additionalProperties = value;
        }

        [Input("properties")]
        private Dictionary<string, Inputs.PropertyDefinition0>? _properties;

        /// <summary>
        /// A map from property name to propertySpec that describes the object's properties.
        /// </summary>
        public Dictionary<string, Inputs.PropertyDefinition0> Properties
        {
            get => _properties ?? (_properties = new Dictionary<string, Inputs.PropertyDefinition0>());
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

        public ObjectTypeDetails0()
        {
        }
        public static new ObjectTypeDetails0 Empty => new ObjectTypeDetails0();
    }
}
