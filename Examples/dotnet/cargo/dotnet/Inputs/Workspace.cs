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
    /// The `[workspace]` table in `Cargo.toml` defines which packages are members of
    /// the workspace:
    /// 
    /// ```toml
    /// [workspace]
    /// members = ["member1", "path/to/member2", "crates/*"]
    /// exclude = ["crates/foo", "path/to/other"]
    /// ```
    /// 
    /// An empty `[workspace]` table can be used with a `[package]` to conveniently
    /// create a workspace with the package and all of its path dependencies.
    /// 
    /// All [`path` dependencies](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html#specifying-path-dependencies) residing in the workspace directory automatically
    /// become members. Additional members can be listed with the `members` key, which
    /// should be an array of strings containing directories with `Cargo.toml` files.
    /// 
    /// The `members` list also supports [globs](https://docs.rs/glob/0.3.0/glob/struct.Pattern.html) to match multiple paths, using
    /// typical filename glob patterns like `*` and `?`.
    /// 
    /// The `exclude` key can be used to prevent paths from being included in a
    /// workspace. This can be useful if some path dependencies aren't desired to be
    /// in the workspace at all, or using a glob pattern and you want to remove a
    /// directory.
    /// 
    /// An empty `[workspace]` table can be used with a `[package]` to conveniently
    /// create a workspace with the package and all of its path dependencies.
    /// </summary>
    public sealed class Workspace : global::Pulumi.InvokeArgs
    {
        [Input("additionalProperties")]
        private Dictionary<string, object>? _additionalProperties;
        public Dictionary<string, object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new Dictionary<string, object>());
            set => _additionalProperties = value;
        }

        [Input("defaultMembers")]
        private List<string>? _defaultMembers;

        /// <summary>
        /// The optional `default-members` key can be specified to set the members to
        /// operate on when in the workspace root and the package selection flags are not
        /// used:
        /// 
        /// ```toml
        /// [workspace]
        /// members = ["path/to/member1", "path/to/member2", "path/to/member3/*"]
        /// default-members = ["path/to/member2", "path/to/member3/foo"]
        /// ```
        /// 
        /// When specified, `default-members` must expand to a subset of `members`.
        /// </summary>
        public List<string> DefaultMembers
        {
            get => _defaultMembers ?? (_defaultMembers = new List<string>());
            set => _defaultMembers = value;
        }

        [Input("dependencies")]
        private Dictionary<string, object>? _dependencies;

        /// <summary>
        /// The `workspace.dependencies` table is where you define dependencies to be
        /// inherited by members of a workspace.
        /// 
        /// Specifying a workspace dependency is similar to [package dependencies](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html) except:
        /// - Dependencies from this table cannot be declared as `optional`
        /// - [`features`][features] declared in this table are additive with the `features` from `[dependencies]`
        /// 
        /// You can then [inherit the workspace dependency as a package dependency](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html#inheriting-a-dependency-from-a-workspace)
        /// 
        /// Example:
        /// ```toml
        /// # [PROJECT_DIR]/Cargo.toml
        /// [workspace]
        /// members = ["bar"]
        /// 
        /// [workspace.dependencies]
        /// cc = "1.0.73"
        /// rand = "0.8.5"
        /// regex = { version = "1.6.0", default-features = false, features = ["std"] }
        /// ```
        /// 
        /// ```toml
        /// # [PROJECT_DIR]/bar/Cargo.toml
        /// [package]
        /// name = "bar"
        /// version = "0.2.0"
        /// 
        /// [dependencies]
        /// regex = { workspace = true, features = ["unicode"] }
        /// 
        /// [build-dependencies]
        /// cc.workspace = true
        /// 
        /// [dev-dependencies]
        /// rand.workspace = true
        /// ```
        /// </summary>
        public Dictionary<string, object> Dependencies
        {
            get => _dependencies ?? (_dependencies = new Dictionary<string, object>());
            set => _dependencies = value;
        }

        [Input("exclude")]
        private List<string>? _exclude;

        /// <summary>
        /// The `exclude` key can be used to prevent paths from being included in a
        /// workspace. This can be useful if some path dependencies aren't desired to be
        /// in the workspace at all, or using a glob pattern and you want to remove a
        /// directory.
        /// </summary>
        public List<string> Exclude
        {
            get => _exclude ?? (_exclude = new List<string>());
            set => _exclude = value;
        }

        [Input("members")]
        private List<string>? _members;

        /// <summary>
        /// All [`path` dependencies] residing in the workspace directory automatically
        /// become members. Additional members can be listed with the `members` key, which
        /// should be an array of strings containing directories with `Cargo.toml` files.
        /// 
        /// The `members` list also supports [globs] to match multiple paths, using
        /// typical filename glob patterns like `*` and `?`.
        /// </summary>
        public List<string> Members
        {
            get => _members ?? (_members = new List<string>());
            set => _members = value;
        }

        [Input("metadata")]
        private Dictionary<string, object>? _metadata;

        /// <summary>
        /// The `workspace.metadata` table is ignored by Cargo and will not be warned
        /// about. This section can be used for tools that would like to store workspace
        /// configuration in `Cargo.toml`. For example:
        /// 
        /// ```toml
        /// [workspace]
        /// members = ["member1", "member2"]
        /// 
        /// [workspace.metadata.webcontents]
        /// root = "path/to/webproject"
        /// tool = ["npm", "run", "build"]
        /// # ...
        /// ```
        /// 
        /// There is a similar set of tables at the package level at
        /// `package.metadata`. While cargo does not specify a
        /// format for the content of either of these tables, it is suggested that
        /// external tools may wish to use them in a consistent fashion, such as referring
        /// to the data in `workspace.metadata` if data is missing from `package.metadata`,
        /// if that makes sense for the tool in question.
        /// </summary>
        public Dictionary<string, object> Metadata
        {
            get => _metadata ?? (_metadata = new Dictionary<string, object>());
            set => _metadata = value;
        }

        /// <summary>
        /// The `workspace.package` table is where you define keys that can be
        /// inherited by members of a workspace. These keys can be inherited by
        /// defining them in the member package with `{key}.workspace = true`.
        /// 
        /// Keys that are supported:
        /// 
        /// |                |                 |
        /// |----------------|-----------------|
        /// | `authors`      | `categories`    |
        /// | `description`  | `documentation` |
        /// | `edition`      | `exclude`       |
        /// | `homepage`     | `include`       |
        /// | `keywords`     | `license`       |
        /// | `license-file` | `publish`       |
        /// | `readme`       | `repository`    |
        /// | `rust-version` | `version`       |
        /// 
        /// - `license-file` and `readme` are relative to the workspace root
        /// - `include` and `exclude` are relative to your package root
        /// 
        /// Example:
        /// ```toml
        /// # [PROJECT_DIR]/Cargo.toml
        /// [workspace]
        /// members = ["bar"]
        /// 
        /// [workspace.package]
        /// version = "1.2.3"
        /// authors = ["Nice Folks"]
        /// description = "A short description of my package"
        /// documentation = "https://example.com/bar"
        /// ```
        /// 
        /// ```toml
        /// # [PROJECT_DIR]/bar/Cargo.toml
        /// [package]
        /// name = "bar"
        /// version.workspace = true
        /// authors.workspace = true
        /// description.workspace = true
        /// documentation.workspace = true
        /// ```
        /// </summary>
        [Input("package")]
        public Inputs.PropertiesPackage? Package { get; set; }

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
        public Pulumi.Cargo.DefinitionsResolver? Resolver { get; set; }

        public Workspace()
        {
        }
        public static new Workspace Empty => new Workspace();
    }
}
