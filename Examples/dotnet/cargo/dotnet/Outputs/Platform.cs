// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.Cargo.Outputs
{

    [OutputType]
    public sealed class Platform
    {
        public readonly ImmutableDictionary<string, object>? AdditionalProperties;
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
        public readonly ImmutableDictionary<string, object>? BuildDependencies;
        public readonly ImmutableDictionary<string, object>? BuildDependencies0;
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
        public readonly ImmutableDictionary<string, object>? Dependencies;
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
        public readonly ImmutableDictionary<string, object>? DevDependencies;
        public readonly ImmutableDictionary<string, object>? DevDependencies0;

        [OutputConstructor]
        private Platform(
            ImmutableDictionary<string, object>? additionalProperties,

            ImmutableDictionary<string, object>? buildDependencies,

            ImmutableDictionary<string, object>? buildDependencies0,

            ImmutableDictionary<string, object>? dependencies,

            ImmutableDictionary<string, object>? devDependencies,

            ImmutableDictionary<string, object>? devDependencies0)
        {
            AdditionalProperties = additionalProperties;
            BuildDependencies = buildDependencies;
            BuildDependencies0 = buildDependencies0;
            Dependencies = dependencies;
            DevDependencies = devDependencies;
            DevDependencies0 = devDependencies0;
        }
    }
}
