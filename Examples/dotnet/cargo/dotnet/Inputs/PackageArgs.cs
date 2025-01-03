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
    /// The only field required by Cargo is [`name`](https://doc.rust-lang.org/cargo/reference/manifest.html#the-name-field).
    ///  If publishing to a registry, the registry may
    /// require additional fields. See the notes below and [the publishing chapter](https://doc.rust-lang.org/cargo/reference/publishing.html) for requirements for publishing to [crates.io](https://crates.io/).
    /// </summary>
    public sealed class PackageArgs : global::Pulumi.ResourceArgs
    {
        [Input("additionalProperties")]
        private InputMap<object>? _additionalProperties;
        public InputMap<object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new InputMap<object>());
            set => _additionalProperties = value;
        }

        /// <summary>
        /// unhandled schema: Json.Schema.AnyOfKeyword
        /// </summary>
        [Input("authors")]
        public Input<object>? Authors { get; set; }

        /// <summary>
        /// Disable automatic discovery of `bench` targets.
        /// 
        /// Disabling automatic discovery should only be needed for specialized
        /// situations. For example, if you have a library where you want a *module* named
        /// `bin`, this would present a problem because Cargo would usually attempt to
        /// compile anything in the `bin` directory as an executable. Here is a sample
        /// layout of this scenario:
        /// 
        /// ```
        /// ├── Cargo.toml
        /// └── src
        ///     ├── lib.rs
        ///     └── bin
        ///         └── mod.rs
        /// ```
        /// </summary>
        [Input("autobenches")]
        public Input<bool>? Autobenches { get; set; }

        /// <summary>
        /// Disable automatic discovery of `bin` targets.
        /// 
        /// Disabling automatic discovery should only be needed for specialized
        /// situations. For example, if you have a library where you want a *module* named
        /// `bin`, this would present a problem because Cargo would usually attempt to
        /// compile anything in the `bin` directory as an executable. Here is a sample
        /// layout of this scenario:
        /// 
        /// ```
        /// ├── Cargo.toml
        /// └── src
        ///     ├── lib.rs
        ///     └── bin
        ///         └── mod.rs
        /// ```
        /// 
        /// To prevent Cargo from inferring `src/bin/mod.rs` as an executable, set
        /// this to `false` to disable auto-discovery.
        /// </summary>
        [Input("autobins")]
        public Input<bool>? Autobins { get; set; }

        /// <summary>
        /// Disable automatic discovery of `example` targets.
        /// 
        /// Disabling automatic discovery should only be needed for specialized
        /// situations. For example, if you have a library where you want a *module* named
        /// `bin`, this would present a problem because Cargo would usually attempt to
        /// compile anything in the `bin` directory as an executable. Here is a sample
        /// layout of this scenario:
        /// 
        /// ```
        /// ├── Cargo.toml
        /// └── src
        ///     ├── lib.rs
        ///     └── bin
        ///         └── mod.rs
        /// ```
        /// </summary>
        [Input("autoexamples")]
        public Input<bool>? Autoexamples { get; set; }

        /// <summary>
        /// Disable automatic discovery of `test` targets.
        /// 
        /// Disabling automatic discovery should only be needed for specialized
        /// situations. For example, if you have a library where you want a *module* named
        /// `bin`, this would present a problem because Cargo would usually attempt to
        /// compile anything in the `bin` directory as an executable. Here is a sample
        /// layout of this scenario:
        /// 
        /// ```
        /// ├── Cargo.toml
        /// └── src
        ///     ├── lib.rs
        ///     └── bin
        ///         └── mod.rs
        /// ```
        /// </summary>
        [Input("autotests")]
        public Input<bool>? Autotests { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.TitleKeyword, Json.Schema.DescriptionKeyword, Json.Schema.AnyOfKeyword, Json.Schema.UnrecognizedKeyword
        /// </summary>
        [Input("build")]
        public Input<object>? Build { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.AnyOfKeyword
        /// </summary>
        [Input("categories")]
        public Input<object>? Categories { get; set; }

        /// <summary>
        /// The `default-run` field in the `[package]` section of the manifest can be used
        /// to specify a default binary picked by [`cargo run`](https://doc.rust-lang.org/cargo/commands/cargo-run.html). For example, when there is
        /// both `src/bin/a.rs` and `src/bin/b.rs`:
        /// 
        /// ```toml
        /// [package]
        /// default-run = "a"
        /// ```
        /// </summary>
        [Input("defaultRun")]
        public Input<string>? DefaultRun { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.AnyOfKeyword
        /// </summary>
        [Input("description")]
        public Input<object>? Description { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.AnyOfKeyword
        /// </summary>
        [Input("documentation")]
        public Input<object>? Documentation { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.AnyOfKeyword
        /// </summary>
        [Input("edition")]
        public Input<object>? Edition { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.AnyOfKeyword
        /// </summary>
        [Input("exclude")]
        public Input<object>? Exclude { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.AnyOfKeyword
        /// </summary>
        [Input("homepage")]
        public Input<object>? Homepage { get; set; }

        /// <summary>
        /// Sets whether the current package is a teapot or something else that is not capable of brewing tea.
        /// </summary>
        [Input("imATeapot")]
        public Input<bool>? ImATeapot { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.AnyOfKeyword
        /// </summary>
        [Input("include")]
        public Input<object>? Include { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.AnyOfKeyword
        /// </summary>
        [Input("keywords")]
        public Input<object>? Keywords { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.AnyOfKeyword
        /// </summary>
        [Input("license")]
        public Input<object>? License { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.AnyOfKeyword
        /// </summary>
        [Input("licenseFile")]
        public Input<object>? LicenseFile { get; set; }

        /// <summary>
        /// The `links` field specifies the name of a native library that is being linked
        /// to. More information can be found in the [`links`](https://doc.rust-lang.org/cargo/reference/build-scripts.html#the-links-manifest-key) section of the build
        /// script guide.
        /// 
        /// ```toml
        /// [package]
        /// # ...
        /// links = "foo"
        /// ```
        /// </summary>
        [Input("links")]
        public Input<string>? Links { get; set; }

        [Input("metabuild")]
        private InputList<string>? _metabuild;
        public InputList<string> Metabuild
        {
            get => _metabuild ?? (_metabuild = new InputList<string>());
            set => _metabuild = value;
        }

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
        [Input("metadata")]
        public Input<Inputs.MetadataArgs>? Metadata { get; set; }

        /// <summary>
        /// The package name is an identifier used to refer to the package. It is used
        /// when listed as a dependency in another package, and as the default name of
        /// inferred lib and bin targets.
        /// 
        /// The name must use only [alphanumeric](https://doc.rust-lang.org/std/primitive.char.html#method.is_alphanumeric) characters or `-` or `_`, and cannot be empty.
        /// Note that [`cargo new`](https://doc.rust-lang.org/cargo/commands/cargo-new.html) and [`cargo init`](https://doc.rust-lang.org/cargo/commands/cargo-init.html) impose some additional restrictions on
        /// the package name, such as enforcing that it is a valid Rust identifier and not
        /// a keyword. [crates.io](https://crates.io) imposes even more restrictions, such as
        /// enforcing only ASCII characters, not a reserved name, not a special Windows
        /// name such as "nul", is not too long, etc.
        /// </summary>
        [Input("name", required: true)]
        public Input<string> Name { get; set; } = null!;

        [Input("namespacedFeatures")]
        public Input<bool>? NamespacedFeatures { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.AnyOfKeyword
        /// </summary>
        [Input("publish")]
        public Input<object>? Publish { get; set; }

        [Input("publishLockfile")]
        public Input<bool>? PublishLockfile { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.AnyOfKeyword
        /// </summary>
        [Input("readme")]
        public Input<object>? Readme { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.AnyOfKeyword
        /// </summary>
        [Input("repository")]
        public Input<object>? Repository { get; set; }

        /// <summary>
        /// A different feature resolver algorithm can be used by specifying the resolver version in Cargo.toml like this:
        /// 
        /// [package]
        /// name = "my-package"
        /// version = "1.0.0"
        /// resolver = "2"
        /// 
        /// The version "1" resolver is the original resolver that shipped with Cargo up to version 1.50. The default is "2" if the root package specifies edition = "2021" or a newer edition. Otherwise the default is "1".
        /// 
        /// The version "2" resolver introduces changes in feature unification. See the features chapter for more details.
        /// 
        /// The resolver is a global option that affects the entire workspace. The resolver version in dependencies is ignored, only the value in the top-level package will be used. If using a virtual workspace, the version should be specified in the [workspace] table, for example:
        /// 
        /// [workspace]
        /// members = ["member1", "member2"]
        /// resolver = "2"
        /// </summary>
        [Input("resolver")]
        public Input<Pulumi.Cargo.Resolver>? Resolver { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.AnyOfKeyword
        /// </summary>
        [Input("rustVersion")]
        public Input<object>? RustVersion { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.AnyOfKeyword
        /// </summary>
        [Input("version")]
        public Input<object>? Version { get; set; }

        /// <summary>
        /// The `workspace` field can be used to configure the workspace that this package
        /// will be a member of. If not specified this will be inferred as the first
        /// Cargo.toml with `[workspace]` upwards in the filesystem. Setting this is
        /// useful if the member is not inside a subdirectory of the workspace root.
        /// 
        /// ```toml
        /// [package]
        /// # ...
        /// workspace = "path/to/workspace/root"
        /// ```
        /// 
        /// This field cannot be specified if the manifest already has a `[workspace]`
        /// table defined. That is, a crate cannot both be a root crate in a workspace
        /// (contain `[workspace]`) and also be a member crate of another workspace
        /// (contain `package.workspace`).
        /// 
        /// For more information, see the [workspaces chapter](https://doc.rust-lang.org/cargo/reference/workspaces.html).
        /// </summary>
        [Input("workspace")]
        public Input<string>? Workspace { get; set; }

        public PackageArgs()
        {
        }
        public static new PackageArgs Empty => new PackageArgs();
    }
}
