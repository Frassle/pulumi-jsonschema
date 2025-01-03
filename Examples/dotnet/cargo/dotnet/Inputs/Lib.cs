// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.Cargo.Inputs
{

    public sealed class Lib : global::Pulumi.InvokeArgs
    {
        [Input("additionalProperties")]
        private Dictionary<string, object>? _additionalProperties;
        public Dictionary<string, object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new Dictionary<string, object>());
            set => _additionalProperties = value;
        }

        /// <summary>
        /// The `bench` field indicates whether or not the target is benchmarked by
        /// default by [`cargo bench`](https://doc.rust-lang.org/cargo/commands/cargo-bench.html). The default is `true` for lib, bins, and
        /// benchmarks.
        /// </summary>
        [Input("bench")]
        public bool? Bench { get; set; }

        [Input("crateType")]
        private List<string>? _crateType;

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
        public List<string> CrateType
        {
            get => _crateType ?? (_crateType = new List<string>());
            set => _crateType = value;
        }

        [Input("crateType0")]
        private List<string>? _crateType0;
        public List<string> CrateType0
        {
            get => _crateType0 ?? (_crateType0 = new List<string>());
            set => _crateType0 = value;
        }

        /// <summary>
        /// The `doc` field indicates whether or not the target is included in the
        /// documentation generated by [`cargo doc`](https://doc.rust-lang.org/cargo/commands/cargo-doc.html) by default. The default is `true` for
        /// libraries and binaries.
        /// 
        /// &gt; **Note**: The binary will be skipped if its name is the same as the lib
        /// &gt; target.
        /// </summary>
        [Input("doc")]
        public bool? Doc { get; set; }

        /// <summary>
        /// The `doctest` field indicates whether or not [documentation examples](https://doc.rust-lang.org/rustdoc/documentation-tests.html) are
        /// tested by default by [`cargo test`](https://doc.rust-lang.org/cargo/commands/cargo-test.html). This is only relevant for libraries, it
        /// has no effect on other sections. The default is `true` for the library.
        /// </summary>
        [Input("doctest")]
        public bool? Doctest { get; set; }

        /// <summary>
        /// The `edition` key affects which edition your package is compiled with. Cargo
        /// will always generate packages via [`cargo new`](https://doc.rust-lang.org/cargo/commands/cargo-new.html) with the `edition` key set to the
        /// latest edition. Setting the `edition` key in `[package]` will affect all
        /// targets/crates in the package, including test suites, benchmarks, binaries,
        /// examples, etc.
        /// </summary>
        [Input("edition")]
        public Pulumi.Cargo.LibEdition? Edition { get; set; }

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
        [Input("harness")]
        public bool? Harness { get; set; }

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
        [Input("name")]
        public string? Name { get; set; }

        /// <summary>
        /// The `path` field specifies where the source for the crate is located, relative
        /// to the `Cargo.toml` file.
        /// 
        /// If not specified, the [inferred path](https://doc.rust-lang.org/cargo/reference/cargo-targets.html#target-auto-discovery) is used based on
        /// the target name.
        /// </summary>
        [Input("path")]
        public string? Path { get; set; }

        [Input("plugin")]
        public bool? Plugin { get; set; }

        /// <summary>
        /// The `proc-macro` field indicates that the library is a [procedural macro](https://doc.rust-lang.org/book/ch19-06-macros.html)
        /// ([reference](https://doc.rust-lang.org/reference/procedural-macros.html)). This is only valid for the `[lib]`
        /// target.
        /// </summary>
        [Input("procMacro")]
        public bool? ProcMacro { get; set; }

        [Input("procMacro0")]
        public bool? ProcMacro0 { get; set; }

        [Input("requiredFeatures")]
        private List<string>? _requiredFeatures;

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
        public List<string> RequiredFeatures
        {
            get => _requiredFeatures ?? (_requiredFeatures = new List<string>());
            set => _requiredFeatures = value;
        }

        /// <summary>
        /// The `test` field indicates whether or not the target is tested by default by
        /// [`cargo test`](https://doc.rust-lang.org/cargo/commands/cargo-test.html). The default is `true` for lib, bins, and tests.
        /// 
        /// &gt; **Note**: Examples are built by [`cargo test`](https://doc.rust-lang.org/cargo/commands/cargo-test.html) by default to ensure they
        /// &gt; continue to compile, but they are not *tested* by default. Setting `test =
        /// &gt; true` for an example will also build it as a test and run any
        /// &gt; [`#[test]`](https://doc.rust-lang.org/reference/attributes/testing.html#the-test-attribute) functions defined in the example.
        /// </summary>
        [Input("test")]
        public bool? Test { get; set; }

        public Lib()
        {
        }
        public static new Lib Empty => new Lib();
    }
}
