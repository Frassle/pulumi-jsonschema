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
    /// A schema for Cargo.toml.
    /// </summary>
    public sealed class Root : global::Pulumi.InvokeArgs
    {
        [Input("additionalProperties")]
        private Dictionary<string, object>? _additionalProperties;
        public Dictionary<string, object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new Dictionary<string, object>());
            set => _additionalProperties = value;
        }

        [Input("badges")]
        private Dictionary<string, ImmutableDictionary<string, string>>? _badges;

        /// <summary>
        /// [crates.io](https://crates.io) can display various badges for build status, test coverage, etc. for
        /// each crate. All badges are optional.
        /// 
        /// - The badges pertaining to build status that are currently available are
        ///   Appveyor, CircleCI, Cirrus CI, GitLab, Azure DevOps, Travis CI and Bitbucket
        ///   Pipelines.
        /// - Available badges pertaining to code test coverage are Codecov and Coveralls.
        /// - There are also maintenance-related badges based on isitmaintained.com
        ///   which state the issue resolution time, percent of open issues, and future
        ///   maintenance intentions.
        /// 
        /// Most badge specifications require a `repository` key. It is expected to be in
        /// `user/repo` format.
        /// 
        /// ```toml
        /// [badges]
        /// 
        /// # Appveyor: `repository` is required. `branch` is optional; default is `master`
        /// # `service` is optional; valid values are `github` (default), `bitbucket`, and
        /// # `gitlab`; `id` is optional; you can specify the appveyor project id if you
        /// # want to use that instead. `project_name` is optional; use when the repository
        /// # name differs from the appveyor project name.
        /// appveyor = { repository = "...", branch = "master", service = "github" }
        /// 
        /// # Circle CI: `repository` is required. `branch` is optional; default is `master`
        /// circle-ci = { repository = "...", branch = "master" }
        /// 
        /// # Cirrus CI: `repository` is required. `branch` is optional; default is `master`
        /// cirrus-ci = { repository = "...", branch = "master" }
        /// 
        /// # GitLab: `repository` is required. `branch` is optional; default is `master`
        /// gitlab = { repository = "...", branch = "master" }
        /// 
        /// # Azure DevOps: `project` is required. `pipeline` is required. `build` is optional; default is `1`
        /// # Note: project = `organization/project`, pipeline = `name_of_pipeline`, build = `definitionId`
        /// azure-devops = { project = "...", pipeline = "...", build="2" }
        /// 
        /// # Travis CI: `repository` in format "&lt;user&gt;/&lt;project&gt;" is required.
        /// # `branch` is optional; default is `master`
        /// travis-ci = { repository = "...", branch = "master" }
        /// 
        /// # Bitbucket Pipelines: `repository` is required. `branch` is required
        /// bitbucket-pipelines = { repository = "...", branch = "master" }
        /// 
        /// # Codecov: `repository` is required. `branch` is optional; default is `master`
        /// # `service` is optional; valid values are `github` (default), `bitbucket`, and
        /// # `gitlab`.
        /// codecov = { repository = "...", branch = "master", service = "github" }
        /// 
        /// # Coveralls: `repository` is required. `branch` is optional; default is `master`
        /// # `service` is optional; valid values are `github` (default) and `bitbucket`.
        /// coveralls = { repository = "...", branch = "master", service = "github" }
        /// 
        /// # Is it maintained resolution time: `repository` is required.
        /// is-it-maintained-issue-resolution = { repository = "..." }
        /// 
        /// # Is it maintained percentage of open issues: `repository` is required.
        /// is-it-maintained-open-issues = { repository = "..." }
        /// 
        /// # Maintenance: `status` is required. Available options are:
        /// # - `actively-developed`: New features are being added and bugs are being fixed.
        /// # - `passively-maintained`: There are no plans for new features, but the maintainer intends to
        /// #   respond to issues that get filed.
        /// # - `as-is`: The crate is feature complete, the maintainer does not intend to continue working on
        /// #   it or providing support, but it works for the purposes it was designed for.
        /// # - `experimental`: The author wants to share it with the community but is not intending to meet
        /// #   anyone's particular use case.
        /// # - `looking-for-maintainer`: The current maintainer would like to transfer the crate to someone
        /// #   else.
        /// # - `deprecated`: The maintainer does not recommend using this crate (the description of the crate
        /// #   can describe why, there could be a better solution available or there could be problems with
        /// #   the crate that the author does not want to fix).
        /// # - `none`: Displays no badge on crates.io, since the maintainer has not chosen to specify
        /// #   their intentions, potential crate users will need to investigate on their own.
        /// maintenance = { status = "..." }
        /// ```
        /// </summary>
        public Dictionary<string, ImmutableDictionary<string, string>> Badges
        {
            get => _badges ?? (_badges = new Dictionary<string, ImmutableDictionary<string, string>>());
            set => _badges = value;
        }

        [Input("bench")]
        private List<Inputs.Target>? _bench;

        /// <summary>
        /// Benchmarks provide a way to test the performance of your code using the
        /// [`cargo bench`](https://doc.rust-lang.org/cargo/commands/cargo-bench.html) command. They follow the same structure as [tests](https://doc.rust-lang.org/cargo/reference/cargo-targets.html#tests),
        /// with each benchmark function annotated with the `#[bench]` attribute.
        /// Similarly to tests:
        /// 
        /// * Benchmarks are placed in the [`benches` directory](https://doc.rust-lang.org/cargo/guide/project-layout.html).
        /// * Benchmark functions defined in libraries and binaries have access to the
        ///   *private* API within the target they are defined in. Benchmarks in the
        ///   `benches` directory may use the *public* API.
        /// * [The `bench` field](https://doc.rust-lang.org/cargo/reference/cargo-targets.html#the-bench-field) can be used to define which targets
        ///   are benchmarked by default.
        /// * [The `harness` field](https://doc.rust-lang.org/cargo/reference/cargo-targets.html#the-harness-field) can be used to disable the
        ///   built-in harness.
        /// 
        /// &gt; **Note**: The [`#[bench]`
        /// &gt; attribute](https://doc.rust-lang.org/unstable-book/library-features/test.html) is currently
        /// &gt; unstable and only available on the [nightly channel](https://doc.rust-lang.org/book/appendix-07-nightly-rust.html). There are some
        /// &gt; packages available on [crates.io](https://crates.io/keywords/benchmark) that
        /// &gt; may help with running benchmarks on the stable channel, such as
        /// &gt; [Criterion](https://crates.io/crates/criterion).
        /// </summary>
        public List<Inputs.Target> Bench
        {
            get => _bench ?? (_bench = new List<Inputs.Target>());
            set => _bench = value;
        }

        [Input("bin")]
        private List<Inputs.DefinitionsTarget>? _bin;

        /// <summary>
        /// Binary targets are executable programs that can be run after being compiled.
        /// The default binary filename is `src/main.rs`, which defaults to the name of
        /// the package. Additional binaries are stored in the [`src/bin/`
        /// directory](https://doc.rust-lang.org/cargo/guide/project-layout.html). The settings for each binary can be [customized](https://doc.rust-lang.org/cargo/reference/cargo-targets.html#configuring-a-target) in the `[[bin]]` tables in `Cargo.toml`.
        /// 
        /// Binaries can use the public API of the package's library. They are also linked
        /// with the [`[dependencies]`](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html) defined in `Cargo.toml`.
        /// 
        /// You can run individual binaries with the [`cargo run`](https://doc.rust-lang.org/cargo/commands/cargo-run.html) command with the `--bin
        /// &lt;bin-name&gt;` option. [`cargo install`](https://doc.rust-lang.org/cargo/commands/cargo-install.html) can be used to copy the executable to a
        /// common location.
        /// 
        /// ```toml
        /// # Example of customizing binaries in Cargo.toml.
        /// [[bin]]
        /// name = "cool-tool"
        /// test = false
        /// bench = false
        /// 
        /// [[bin]]
        /// name = "frobnicator"
        /// required-features = ["frobnicate"]
        /// ```
        /// </summary>
        public List<Inputs.DefinitionsTarget> Bin
        {
            get => _bin ?? (_bin = new List<Inputs.DefinitionsTarget>());
            set => _bin = value;
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

        [Input("cargoFeatures")]
        private List<string>? _cargoFeatures;
        public List<string> CargoFeatures
        {
            get => _cargoFeatures ?? (_cargoFeatures = new List<string>());
            set => _cargoFeatures = value;
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

        [Input("example")]
        private List<Inputs.Items>? _example;

        /// <summary>
        /// Files located under the [examples directory](https://doc.rust-lang.org/cargo/guide/project-layout.html) are example uses of the functionality provided by the library. When compiled, they are placed in the[ target/debug/examples directory](https://doc.rust-lang.org/cargo/guide/build-cache.html).
        /// 
        /// Examples can use the public API of the package's library. They are also linked with the [dependencies](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html) and [dev-dependencies](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html#development-dependencies) defined in Cargo.toml.
        /// 
        /// By default, examples are executable binaries (with a `main()` function). You
        /// can specify the [`crate-type` field](https://doc.rust-lang.org/cargo/reference/cargo-targets.html#the-crate-type-field) to make an example
        /// be compiled as a library:
        /// 
        /// ```toml
        /// [[example]]
        /// name = "foo"
        /// crate-type = ["staticlib"]
        /// ```
        /// 
        /// You can run individual executable examples with the [`cargo run`](https://doc.rust-lang.org/cargo/commands/cargo-run.html) command with
        /// the `--example &lt;example-name&gt;` option. Library examples can be built with
        /// [`cargo build`](https://doc.rust-lang.org/cargo/commands/cargo-build.html) with the `--example &lt;example-name&gt;` option. [`cargo install`](https://doc.rust-lang.org/cargo/commands/cargo-install.html)
        /// with the `--example &lt;example-name&gt;` option can be used to copy executable
        /// binaries to a common location. Examples are compiled by [`cargo test`](https://doc.rust-lang.org/cargo/commands/cargo-test.html) by
        /// default to protect them from bit-rotting. Set [the `test`
        /// field](https://doc.rust-lang.org/cargo/reference/cargo-targets.html#the-test-field) to `true` if you have `#[test]` functions in the
        /// example that you want to run with [`cargo test`](https://doc.rust-lang.org/cargo/commands/cargo-test.html).
        /// </summary>
        public List<Inputs.Items> Example
        {
            get => _example ?? (_example = new List<Inputs.Items>());
            set => _example = value;
        }

        [Input("features")]
        private Dictionary<string, ImmutableArray<string>>? _features;

        /// <summary>
        /// Cargo supports features to allow expression of:
        /// 
        /// * conditional compilation options (usable through `cfg` attributes);
        /// * optional dependencies, which enhance a package, but are not required; and
        /// * clusters of optional dependencies, such as `postgres-all`, that would include the
        ///   `postgres` package, the `postgres-macros` package, and possibly other packages
        ///   (such as development-time mocking libraries, debugging tools, etc.).
        /// 
        /// A feature of a package is either an optional dependency, or a set of other
        /// features.
        /// </summary>
        public Dictionary<string, ImmutableArray<string>> Features
        {
            get => _features ?? (_features = new Dictionary<string, ImmutableArray<string>>());
            set => _features = value;
        }

        [Input("lib")]
        public Inputs.Lib? Lib { get; set; }

        [Input("package")]
        public Inputs.Package? Package { get; set; }

        [Input("patch")]
        private Dictionary<string, ImmutableDictionary<string, object>>? _patch;

        /// <summary>
        /// The `[patch]` section of `Cargo.toml` can be used to override dependencies
        /// with other copies. The syntax is similar to the
        /// [`[dependencies]`](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html) section.
        /// </summary>
        public Dictionary<string, ImmutableDictionary<string, object>> Patch
        {
            get => _patch ?? (_patch = new Dictionary<string, ImmutableDictionary<string, object>>());
            set => _patch = value;
        }

        [Input("profile")]
        public Inputs.Profiles? Profile { get; set; }

        /// <summary>
        /// Ref needs more translation map [(Json.Schema.UnrecognizedKeyword, [Json.Schema.UnrecognizedKeyword; Json.Schema.UnrecognizedKeyword])]
        /// </summary>
        [Input("project")]
        public object? Project { get; set; }

        [Input("replace")]
        private Dictionary<string, object>? _replace;
        public Dictionary<string, object> Replace
        {
            get => _replace ?? (_replace = new Dictionary<string, object>());
            set => _replace = value;
        }

        [Input("target")]
        private Dictionary<string, Inputs.Platform>? _target;
        public Dictionary<string, Inputs.Platform> Target
        {
            get => _target ?? (_target = new Dictionary<string, Inputs.Platform>());
            set => _target = value;
        }

        [Input("test")]
        private List<Inputs.TestItems>? _test;

        /// <summary>
        /// Files located under the [`tests` directory](https://doc.rust-lang.org/cargo/guide/project-layout.html) are integration
        /// tests. When you run [`cargo test`](https://doc.rust-lang.org/cargo/commands/cargo-test.html), Cargo will compile each of these files as
        /// a separate crate, and execute them.
        /// 
        /// Integration tests can use the public API of the package's library. They are
        /// also linked with the [`[dependencies]`](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html) and
        /// [`[dev-dependencies]`](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html#development-dependencies) defined in `Cargo.toml`.
        /// 
        /// If you want to share code among multiple integration tests, you can place it
        /// in a separate module such as `tests/common/mod.rs` and then put `mod common;`
        /// in each test to import it.
        /// 
        /// Each integration test results in a separate executable binary, and [`cargo
        /// test`](https://doc.rust-lang.org/cargo/commands/cargo-test.html) will run them serially. In some cases this can be inefficient, as it
        /// can take longer to compile, and may not make full use of multiple CPUs when
        /// running the tests. If you have a lot of integration tests, you may want to
        /// consider creating a single integration test, and split the tests into multiple
        /// modules. The libtest harness will automatically find all of the `#[test]`
        /// annotated functions and run them in parallel. You can pass module names to
        /// [`cargo test`](https://doc.rust-lang.org/cargo/commands/cargo-test.html) to only run the tests within that module.
        /// 
        /// Binary targets are automatically built if there is an integration test. This
        /// allows an integration test to execute the binary to exercise and test its
        /// behavior. The `CARGO_BIN_EXE_&lt;name&gt;` [environment variable](https://doc.rust-lang.org/cargo/reference/environment-variables.html#environment-variables-cargo-sets-for-crates) is set when the
        /// integration test is built so that it can use the [`env` macro](https://doc.rust-lang.org/std/macro.env.html) to locate the
        /// executable.
        /// </summary>
        public List<Inputs.TestItems> Test
        {
            get => _test ?? (_test = new List<Inputs.TestItems>());
            set => _test = value;
        }

        [Input("workspace")]
        public Inputs.Workspace? Workspace { get; set; }

        public Root()
        {
        }
        public static new Root Empty => new Root();
    }
}
