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
    public sealed class ResourceDefinition0Args : global::Pulumi.ResourceArgs
    {
        [Input("additionalProperties")]
        private InputMap<object>? _additionalProperties;
        public InputMap<object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new InputMap<object>());
            set => _additionalProperties = value;
        }

        [Input("aliases")]
        private InputList<Inputs.AliasDefinitionArgs>? _aliases;

        /// <summary>
        /// The list of aliases for the resource.
        /// </summary>
        public InputList<Inputs.AliasDefinitionArgs> Aliases
        {
            get => _aliases ?? (_aliases = new InputList<Inputs.AliasDefinitionArgs>());
            set => _aliases = value;
        }

        /// <summary>
        /// Indicates whether the resource is deprecated
        /// </summary>
        [Input("deprecationMessage")]
        public Input<string>? DeprecationMessage { get; set; }

        /// <summary>
        /// The description of the resource, if any. Interpreted as Markdown.
        /// </summary>
        [Input("description")]
        public Input<string>? Description { get; set; }

        [Input("inputProperties")]
        private InputMap<Inputs.PropertyDefinition5Args>? _inputProperties;

        /// <summary>
        /// A map from property name to propertySpec that describes the resource's input properties.
        /// </summary>
        public InputMap<Inputs.PropertyDefinition5Args> InputProperties
        {
            get => _inputProperties ?? (_inputProperties = new InputMap<Inputs.PropertyDefinition5Args>());
            set => _inputProperties = value;
        }

        /// <summary>
        /// Indicates whether the resource is a component.
        /// </summary>
        [Input("isComponent")]
        public Input<bool>? IsComponent { get; set; }

        /// <summary>
        /// Indicates that the implementation of the resource should not be generated from the schema, and is instead provided out-of-band by the package author
        /// </summary>
        [Input("isOverlay")]
        public Input<bool>? IsOverlay { get; set; }

        [Input("methods")]
        private InputMap<string>? _methods;

        /// <summary>
        /// A map from method name to function token that describes the resource's method set.
        /// </summary>
        public InputMap<string> Methods
        {
            get => _methods ?? (_methods = new InputMap<string>());
            set => _methods = value;
        }

        [Input("properties")]
        private InputMap<Inputs.PropertyDefinition1Args>? _properties;

        /// <summary>
        /// A map from property name to propertySpec that describes the object's properties.
        /// </summary>
        public InputMap<Inputs.PropertyDefinition1Args> Properties
        {
            get => _properties ?? (_properties = new InputMap<Inputs.PropertyDefinition1Args>());
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

        [Input("requiredInputs")]
        private InputList<string>? _requiredInputs;

        /// <summary>
        /// A list of the names of the resource's required input properties.
        /// </summary>
        public InputList<string> RequiredInputs
        {
            get => _requiredInputs ?? (_requiredInputs = new InputList<string>());
            set => _requiredInputs = value;
        }

        [Input("stateInputs")]
        public Input<Inputs.ResourceDefinition0Args>? StateInputs { get; set; }

        public ResourceDefinition0Args()
        {
        }
        public static new ResourceDefinition0Args Empty => new ResourceDefinition0Args();
    }
}
