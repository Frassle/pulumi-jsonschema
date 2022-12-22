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
    public sealed class ObjectTypeDetailsArgs : global::Pulumi.ResourceArgs
    {
        [Input("additionalProperties")]
        private InputMap<object>? _additionalProperties;
        public InputMap<object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new InputMap<object>());
            set => _additionalProperties = value;
        }

        [Input("properties")]
        private InputMap<Inputs.PropertiesAdditionalPropertiesArgs>? _properties;

        /// <summary>
        /// A map from property name to propertySpec that describes the object's properties.
        /// </summary>
        public InputMap<Inputs.PropertiesAdditionalPropertiesArgs> Properties
        {
            get => _properties ?? (_properties = new InputMap<Inputs.PropertiesAdditionalPropertiesArgs>());
            set => _properties = value;
        }

        [Input("required")]
        private InputList<string>? _required;

        /// <summary>
        /// A list of the names of an object type's required properties. These properties must be set for inputs and will always be set for outputs.
        /// </summary>
        public InputList<string> Required
        {
            get => _required ?? (_required = new InputList<string>());
            set => _required = value;
        }

        public ObjectTypeDetailsArgs()
        {
        }
        public static new ObjectTypeDetailsArgs Empty => new ObjectTypeDetailsArgs();
    }
}
