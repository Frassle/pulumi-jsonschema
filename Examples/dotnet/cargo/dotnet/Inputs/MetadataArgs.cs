// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.Cargo.Inputs
{

    /// <summary>
    /// Cargo by default will warn about unused keys in `Cargo.toml` to assist in
    /// detecting typos and such. The `package.metadata` table, however, is completely
    /// ignored by Cargo and will not be warned about. This section can be used for
    /// tools which would like to store package configuration in `Cargo.toml`. For
    /// example:
    /// 
    /// ```toml
    /// [package]
    /// name = "..."
    /// # ...
    /// 
    /// # Metadata used when generating an Android APK, for example.
    /// [package.metadata.android]
    /// package-name = "my-awesome-android-app"
    /// assets = "path/to/static"
    /// ```
    /// </summary>
    public sealed class MetadataArgs : global::Pulumi.ResourceArgs
    {
        [Input("additionalProperties")]
        private InputMap<object>? _additionalProperties;
        public InputMap<object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new InputMap<object>());
            set => _additionalProperties = value;
        }

        /// <summary>
        /// Metadata and build configuration.
        /// </summary>
        [Input("playdate")]
        public Input<Inputs.PlaydatePackageMetadataArgs>? Playdate { get; set; }

        public MetadataArgs()
        {
        }
        public static new MetadataArgs Empty => new MetadataArgs();
    }
}
