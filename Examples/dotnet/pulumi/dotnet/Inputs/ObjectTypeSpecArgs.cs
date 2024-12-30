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
    /// Describes a resource or component.
    /// </summary>
    public sealed class ObjectTypeSpecArgs : global::Pulumi.ResourceArgs
    {
        [Input("additionalProperties")]
        private Dictionary<string, object>? _additionalProperties;
        public Dictionary<string, object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new Dictionary<string, object>());
            set => _additionalProperties = value;
        }

        [Input("aliases")]
        private List<Inputs.AliasDefinitionArgs>? _aliases;

        /// <summary>
        /// The list of aliases for the resource.
        /// </summary>
        public List<Inputs.AliasDefinitionArgs> Aliases
        {
            get => _aliases ?? (_aliases = new List<Inputs.AliasDefinitionArgs>());
            set => _aliases = value;
        }

        /// <summary>
        /// Indicates whether the resource is deprecated
        /// </summary>
        [Input("deprecationMessage")]
        public string? DeprecationMessage { get; set; }

        /// <summary>
        /// The description of the resource, if any. Interpreted as Markdown.
        /// </summary>
        [Input("description")]
        public string? Description { get; set; }

        [Input("inputProperties")]
        private Dictionary<string, Inputs.TypeSpecArgs>? _inputProperties;

        /// <summary>
        /// A map from property name to propertySpec that describes the resource's input properties.
        /// </summary>
        public Dictionary<string, Inputs.TypeSpecArgs> InputProperties
        {
            get => _inputProperties ?? (_inputProperties = new Dictionary<string, Inputs.TypeSpecArgs>());
            set => _inputProperties = value;
        }

        /// <summary>
        /// Indicates whether the resource is a component.
        /// </summary>
        [Input("isComponent")]
        public bool? IsComponent { get; set; }

        /// <summary>
        /// Indicates that the implementation of the resource should not be generated from the schema, and is instead provided out-of-band by the package author
        /// </summary>
        [Input("isOverlay")]
        public bool? IsOverlay { get; set; }

        [Input("methods")]
        private Dictionary<string, string>? _methods;

        /// <summary>
        /// A map from method name to function token that describes the resource's method set.
        /// </summary>
        public Dictionary<string, string> Methods
        {
            get => _methods ?? (_methods = new Dictionary<string, string>());
            set => _methods = value;
        }

        [Input("properties")]
        private Dictionary<string, Inputs.ProviderPropertiesAdditionalPropertiesArgs>? _properties;

        /// <summary>
        /// A map from property name to propertySpec that describes the object's properties.
        /// </summary>
        public Dictionary<string, Inputs.ProviderPropertiesAdditionalPropertiesArgs> Properties
        {
            get => _properties ?? (_properties = new Dictionary<string, Inputs.ProviderPropertiesAdditionalPropertiesArgs>());
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

        [Input("requiredInputs")]
        private List<string>? _requiredInputs;

        /// <summary>
        /// A list of the names of the resource's required input properties.
        /// </summary>
        public List<string> RequiredInputs
        {
            get => _requiredInputs ?? (_requiredInputs = new List<string>());
            set => _requiredInputs = value;
        }

        [Input("stateInputs")]
        public Inputs.ObjectTypeSpecArgs? StateInputs { get; set; }

        public ObjectTypeSpecArgs()
        {
        }
        public static new ObjectTypeSpecArgs Empty => new ObjectTypeSpecArgs();
    }
}
