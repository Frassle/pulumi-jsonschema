// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.Cargo.Inputs
{

    public sealed class PlatformArgs : global::Pulumi.ResourceArgs
    {
        [Input("additionalProperties")]
        private Dictionary<string, object>? _additionalProperties;
        public Dictionary<string, object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new Dictionary<string, object>());
            set => _additionalProperties = value;
        }

        [Input("buildDependencies")]
        private Dictionary<string, object>? _buildDependencies;

        /// <summary>
        /// You can depend on other Cargo-based crates for use in your build scripts.
        /// Dependencies are declared through the `build-dependencies` section of the
        /// manifest:
        /// 
        /// ```toml
        /// [build-dependencies]
        /// cc = "1.0.3"
        /// ```
        /// 
        /// The build script **does not** have access to the dependencies listed
        /// in the `dependencies` or `dev-dependencies` section. Build
        /// dependencies will likewise not be available to the package itself
        /// unless listed under the `dependencies` section as well. A package
        /// itself and its build script are built separately, so their
        /// dependencies need not coincide. Cargo is kept simpler and cleaner by
        /// using independent dependencies for independent purposes.
        /// </summary>
        public Dictionary<string, object> BuildDependencies
        {
            get => _buildDependencies ?? (_buildDependencies = new Dictionary<string, object>());
            set => _buildDependencies = value;
        }

        [Input("buildDependencies0")]
        private Dictionary<string, object>? _buildDependencies0;
        public Dictionary<string, object> BuildDependencies0
        {
            get => _buildDependencies0 ?? (_buildDependencies0 = new Dictionary<string, object>());
            set => _buildDependencies0 = value;
        }

        [Input("dependencies")]
        private Dictionary<string, object>? _dependencies;

        /// <summary>
        /// Cargo is configured to look for dependencies on [crates.io](https://crates.io) by default. Only
        /// the name and a version string are required in this case. In [the cargo
        /// guide](https://doc.rust-lang.org/cargo/guide/index.html), we specified a dependency on the `time` crate:
        /// 
        /// ```toml
        /// [dependencies]
        /// time = "0.1.12"
        /// ```
        /// 
        /// The string `"0.1.12"` is a [semver](https://github.com/steveklabnik/semver#requirements) version requirement. Since this
        /// string does not have any operators in it, it is interpreted the same way as
        /// if we had specified `"^0.1.12"`, which is called a caret requirement.
        /// 
        /// A dependency can also be defined by a table with additional options:
        /// 
        /// ```toml
        /// [dependencies]
        /// time = { path = "../time", version = "0.1.12" }
        /// ```
        /// </summary>
        public Dictionary<string, object> Dependencies
        {
            get => _dependencies ?? (_dependencies = new Dictionary<string, object>());
            set => _dependencies = value;
        }

        [Input("devDependencies")]
        private Dictionary<string, object>? _devDependencies;

        /// <summary>
        /// The format of `[dev-dependencies]` is equivalent to `[dependencies]`:
        /// 
        /// ```toml
        /// [dev-dependencies]
        /// tempdir = "0.3"
        /// ```
        /// 
        /// Dev-dependencies are not used when compiling
        /// a package for building, but are used for compiling tests, examples, and
        /// benchmarks.
        /// 
        /// These dependencies are *not* propagated to other packages which depend on this
        /// package.
        /// 
        /// You can also have target-specific development dependencies by using
        /// `dev-dependencies` in the target section header instead of `dependencies`. For
        /// example:
        /// 
        /// ```toml
        /// [target.'cfg(unix)'.dev-dependencies]
        /// mio = "0.0.1"
        /// ```
        /// 
        /// &gt; **Note**: When a package is published, only dev-dependencies that specify a
        /// &gt; `version` will be included in the published crate. For most use cases,
        /// &gt; dev-dependencies are not needed when published, though some users (like OS
        /// &gt; packagers) may want to run tests within a crate, so providing a `version` if
        /// &gt; possible can still be beneficial.
        /// </summary>
        public Dictionary<string, object> DevDependencies
        {
            get => _devDependencies ?? (_devDependencies = new Dictionary<string, object>());
            set => _devDependencies = value;
        }

        [Input("devDependencies0")]
        private Dictionary<string, object>? _devDependencies0;
        public Dictionary<string, object> DevDependencies0
        {
            get => _devDependencies0 ?? (_devDependencies0 = new Dictionary<string, object>());
            set => _devDependencies0 = value;
        }

        public PlatformArgs()
        {
        }
        public static new PlatformArgs Empty => new PlatformArgs();
    }
}
