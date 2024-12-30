// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.Cargo.Outputs
{

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
    [OutputType]
    public sealed class Items
    {
        public readonly ImmutableDictionary<string, object>? AdditionalProperties;
        /// <summary>
        /// The `bench` field indicates whether or not the target is benchmarked by
        /// default by [`cargo bench`](https://doc.rust-lang.org/cargo/commands/cargo-bench.html). The default is `true` for lib, bins, and
        /// benchmarks.
        /// </summary>
        public readonly bool? Bench;
        /// <summary>
        /// The `crate-type` field defines the [crate types](https://doc.rust-lang.org/reference/linkage.html) that will be generated by the
        /// target. It is an array of strings, allowing you to specify multiple crate
        /// types for a single target. This can only be specified for libraries and
        /// examples. Binaries, tests, and benchmarks are always the "bin" crate type.
        /// 
        /// The available options are `bin`, `lib`, `rlib`, `dylib`, `cdylib`,
        /// `staticlib`, and `proc-macro`. You can read more about the different crate
        /// types in the [Rust Reference Manual](https://doc.rust-lang.org/reference/linkage.html).
        /// </summary>
        public readonly ImmutableArray<string> CrateType;
        public readonly ImmutableArray<string> CrateType0;
        /// <summary>
        /// The `doc` field indicates whether or not the target is included in the
        /// documentation generated by [`cargo doc`](https://doc.rust-lang.org/cargo/commands/cargo-doc.html) by default. The default is `true` for
        /// libraries and binaries.
        /// 
        /// &gt; **Note**: The binary will be skipped if its name is the same as the lib
        /// &gt; target.
        /// </summary>
        public readonly bool? Doc;
        /// <summary>
        /// The `doctest` field indicates whether or not [documentation examples](https://doc.rust-lang.org/rustdoc/documentation-tests.html) are
        /// tested by default by [`cargo test`](https://doc.rust-lang.org/cargo/commands/cargo-test.html). This is only relevant for libraries, it
        /// has no effect on other sections. The default is `true` for the library.
        /// </summary>
        public readonly bool? Doctest;
        /// <summary>
        /// The `edition` key affects which edition your package is compiled with. Cargo
        /// will always generate packages via [`cargo new`](https://doc.rust-lang.org/cargo/commands/cargo-new.html) with the `edition` key set to the
        /// latest edition. Setting the `edition` key in `[package]` will affect all
        /// targets/crates in the package, including test suites, benchmarks, binaries,
        /// examples, etc.
        /// </summary>
        public readonly Pulumi.Cargo.ItemsEdition? Edition;
        /// <summary>
        /// The `harness` field indicates that the [`--test` flag](https://doc.rust-lang.org/rustc/command-line-arguments.html#option-test) will be passed to
        /// `rustc` which will automatically include the libtest library which is the
        /// driver for collecting and running tests marked with the [`#[test]` attribute](https://doc.rust-lang.org/reference/attributes/testing.html#the-test-attribute) or benchmarks with the `#[bench]` attribute. The
        /// default is `true` for all targets.
        /// 
        /// If set to `false`, then you are responsible for defining a `main()` function
        /// to run tests and benchmarks.
        /// 
        /// Tests have the [`cfg(test)` conditional expression](https://doc.rust-lang.org/reference/conditional-compilation.html#test) enabled whether
        /// or not the harness is enabled.
        /// </summary>
        public readonly bool? Harness;
        /// <summary>
        /// The `name` field specifies the name of the target, which corresponds to the
        /// filename of the artifact that will be generated. For a library, this is the
        /// crate name that dependencies will use to reference it.
        /// 
        /// For the `[lib]` and the default binary (`src/main.rs`), this defaults to the
        /// name of the package, with any dashes replaced with underscores. For other
        /// [auto discovered](https://doc.rust-lang.org/cargo/reference/cargo-targets.html#target-auto-discovery) targets, it defaults to the
        /// directory or file name.
        /// 
        /// This is required for all targets except `[lib]`.
        /// </summary>
        public readonly string? Name;
        /// <summary>
        /// The `path` field specifies where the source for the crate is located, relative
        /// to the `Cargo.toml` file.
        /// 
        /// If not specified, the [inferred path](https://doc.rust-lang.org/cargo/reference/cargo-targets.html#target-auto-discovery) is used based on
        /// the target name.
        /// </summary>
        public readonly string? Path;
        public readonly bool? Plugin;
        /// <summary>
        /// The `proc-macro` field indicates that the library is a [procedural macro](https://doc.rust-lang.org/book/ch19-06-macros.html)
        /// ([reference](https://doc.rust-lang.org/reference/procedural-macros.html)). This is only valid for the `[lib]`
        /// target.
        /// </summary>
        public readonly bool? ProcMacro;
        public readonly bool? ProcMacro0;
        /// <summary>
        /// The `required-features` field specifies which [features](https://doc.rust-lang.org/cargo/reference/features.html) the target needs in
        /// order to be built. If any of the required features are not enabled, the
        /// target will be skipped. This is only relevant for the `[[bin]]`, `[[bench]]`,
        /// `[[test]]`, and `[[example]]` sections, it has no effect on `[lib]`.
        /// 
        /// ```toml
        /// [features]
        /// # ...
        /// postgres = []
        /// sqlite = []
        /// tools = []
        /// 
        /// [[bin]]
        /// name = "my-pg-tool"
        /// required-features = ["postgres", "tools"]
        /// ```
        /// </summary>
        public readonly ImmutableArray<string> RequiredFeatures;
        /// <summary>
        /// The `test` field indicates whether or not the target is tested by default by
        /// [`cargo test`](https://doc.rust-lang.org/cargo/commands/cargo-test.html). The default is `true` for lib, bins, and tests.
        /// 
        /// &gt; **Note**: Examples are built by [`cargo test`](https://doc.rust-lang.org/cargo/commands/cargo-test.html) by default to ensure they
        /// &gt; continue to compile, but they are not *tested* by default. Setting `test =
        /// &gt; true` for an example will also build it as a test and run any
        /// &gt; [`#[test]`](https://doc.rust-lang.org/reference/attributes/testing.html#the-test-attribute) functions defined in the example.
        /// </summary>
        public readonly bool? Test;

        [OutputConstructor]
        private Items(
            ImmutableDictionary<string, object>? additionalProperties,

            bool? bench,

            ImmutableArray<string> crateType,

            ImmutableArray<string> crateType0,

            bool? doc,

            bool? doctest,

            Pulumi.Cargo.ItemsEdition? edition,

            bool? harness,

            string? name,

            string? path,

            bool? plugin,

            bool? procMacro,

            bool? procMacro0,

            ImmutableArray<string> requiredFeatures,

            bool? test)
        {
            AdditionalProperties = additionalProperties;
            Bench = bench;
            CrateType = crateType;
            CrateType0 = crateType0;
            Doc = doc;
            Doctest = doctest;
            Edition = edition;
            Harness = harness;
            Name = name;
            Path = path;
            Plugin = plugin;
            ProcMacro = procMacro;
            ProcMacro0 = procMacro0;
            RequiredFeatures = requiredFeatures;
            Test = test;
        }
    }
}
