// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading.Tasks;
using Pulumi.Serialization;

namespace Pulumi.Cargo.Inputs
{

    public sealed class ProfileArgs : global::Pulumi.ResourceArgs
    {
        [Input("additionalProperties")]
        private InputMap<object>? _additionalProperties;
        public InputMap<object> AdditionalProperties
        {
            get => _additionalProperties ?? (_additionalProperties = new InputMap<object>());
            set => _additionalProperties = value;
        }

        [Input("buildOverride")]
        public Input<Inputs.ProfileArgs>? BuildOverride { get; set; }

        /// <summary>
        /// The `codegen-units` setting controls the [`-C codegen-units` flag](https://doc.rust-lang.org/rustc/codegen-options/index.html#codegen-units) which
        /// controls how many "code generation units" a crate will be split into. More
        /// code generation units allows more of a crate to be processed in parallel
        /// possibly reducing compile time, but may produce slower code.
        /// 
        /// This option takes an integer greater than 0.
        /// 
        /// The default is 256 for [incremental](https://doc.rust-lang.org/cargo/reference/profiles.html#incremental) builds, and 16 for
        /// non-incremental builds.
        /// </summary>
        [Input("codegenUnits")]
        public Input<int>? CodegenUnits { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.TitleKeyword, Json.Schema.DescriptionKeyword, Json.Schema.EnumKeyword, Json.Schema.UnrecognizedKeyword
        /// </summary>
        [Input("debug")]
        public Input<object>? Debug { get; set; }

        /// <summary>
        /// The `debug-assertions` setting controls the [`-C debug-assertions` flag](https://doc.rust-lang.org/rustc/codegen-options/index.html#debug-assertions) which
        /// turns `cfg(debug_assertions)` [conditional compilation](https://doc.rust-lang.org/reference/conditional-compilation.html#debug_assertions) on or off. Debug
        /// assertions are intended to include runtime validation which is only available
        /// in debug/development builds. These may be things that are too expensive or
        /// otherwise undesirable in a release build. Debug assertions enables the
        /// [`debug_assert!` macro](https://doc.rust-lang.org/std/macro.debug_assert.html) in the standard library.
        /// </summary>
        [Input("debugAssertions")]
        public Input<bool>? DebugAssertions { get; set; }

        [Input("dirName")]
        public Input<string>? DirName { get; set; }

        /// <summary>
        /// The `incremental` setting controls the [`-C incremental` flag](https://doc.rust-lang.org/rustc/codegen-options/index.html#incremental) which controls
        /// whether or not incremental compilation is enabled. Incremental compilation
        /// causes `rustc` to to save additional information to disk which will be reused
        /// when recompiling the crate, improving re-compile times. The additional
        /// information is stored in the `target` directory.
        /// 
        /// The valid options are:
        /// 
        /// * `true`: enabled
        /// * `false`: disabled
        /// 
        /// Incremental compilation is only used for workspace members and "path"
        /// dependencies.
        /// 
        /// The incremental value can be overridden globally with the `CARGO_INCREMENTAL`
        /// [environment variable](https://doc.rust-lang.org/cargo/reference/environment-variables.html) or the [`build.incremental`](https://doc.rust-lang.org/cargo/reference/config.html#buildincremental) config variable.
        /// </summary>
        [Input("incremental")]
        public Input<bool>? Incremental { get; set; }

        [Input("inherits")]
        public Input<string>? Inherits { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.TitleKeyword, Json.Schema.DescriptionKeyword, Json.Schema.EnumKeyword, Json.Schema.UnrecognizedKeyword
        /// </summary>
        [Input("lto")]
        public Input<object>? Lto { get; set; }

        /// <summary>
        /// unhandled schema: Json.Schema.TitleKeyword, Json.Schema.DescriptionKeyword, Json.Schema.EnumKeyword, Json.Schema.UnrecognizedKeyword
        /// </summary>
        [Input("optLevel")]
        public Input<object>? OptLevel { get; set; }

        /// <summary>
        /// The `overflow-checks` setting controls the [`-C overflow-checks` flag](https://doc.rust-lang.org/rustc/codegen-options/index.html#overflow-checks) which
        /// controls the behavior of [runtime integer overflow](https://doc.rust-lang.org/reference/expressions/operator-expr.html#overflow). When overflow-checks are
        /// enabled, a panic will occur on overflow.
        /// </summary>
        [Input("overflowChecks")]
        public Input<bool>? OverflowChecks { get; set; }

        [Input("package")]
        private InputMap<Inputs.ProfileArgs>? _package;

        /// <summary>
        /// Package-specific overrides.
        /// 
        /// The package name is a [Package ID Spec](https://doc.rust-lang.org/cargo/reference/pkgid-spec.html), so you can
        /// target individual versions of a package with syntax such as `[profile.dev.package."foo:2.1.0"]`.
        /// </summary>
        public InputMap<Inputs.ProfileArgs> Package
        {
            get => _package ?? (_package = new InputMap<Inputs.ProfileArgs>());
            set => _package = value;
        }

        /// <summary>
        /// The `panic` setting controls the [`-C panic` flag](https://doc.rust-lang.org/rustc/codegen-options/index.html#panic) which controls which panic
        /// strategy to use.
        /// 
        /// When set to `"unwind"`, the actual value depends on the default of the target
        /// platform. For example, the NVPTX platform does not support unwinding, so it
        /// always uses `"abort"`.
        /// 
        /// Tests, benchmarks, build scripts, and proc macros ignore the `panic` setting.
        /// The `rustc` test harness currently requires `unwind` behavior. See the
        /// [`panic-abort-tests`](https://doc.rust-lang.org/cargo/reference/unstable.html#panic-abort-tests) unstable flag which enables `abort` behavior.
        /// 
        /// Additionally, when using the `abort` strategy and building a test, all of the
        /// dependencies will also be forced to built with the `unwind` strategy.
        /// </summary>
        [Input("panic")]
        public Input<Pulumi.Cargo.Panic>? Panic { get; set; }

        /// <summary>
        /// The `rpath` setting controls the [`-C rpath` flag](https://doc.rust-lang.org/rustc/codegen-options/index.html#rpath) which controls
        /// whether or not [`rpath`](https://en.wikipedia.org/wiki/Rpath) is enabled.
        /// </summary>
        [Input("rpath")]
        public Input<bool>? Rpath { get; set; }

        public ProfileArgs()
        {
        }
        public static new ProfileArgs Empty => new ProfileArgs();
    }
}
