// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.ComponentModel;
using Pulumi;

namespace Pulumi.Bootstraprc
{
    /// <summary>
    /// The major version of Bootstrap being used
    /// </summary>
    public enum BootstrapVersion
    {
        Three = 3,
        Four = 4,
    }

    /// <summary>
    /// The verbosity of logging. Exclude this property to disable.
    /// </summary>
    [EnumType]
    public readonly struct Loglevel : IEquatable<Loglevel>
    {
        private readonly string _value;

        private Loglevel(string value)
        {
            _value = value ?? throw new ArgumentNullException(nameof(value));
        }

        public static Loglevel Debug { get; } = new Loglevel("debug");

        public static bool operator ==(Loglevel left, Loglevel right) => left.Equals(right);
        public static bool operator !=(Loglevel left, Loglevel right) => !left.Equals(right);

        public static explicit operator string(Loglevel value) => value._value;

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override bool Equals(object? obj) => obj is Loglevel other && Equals(other);
        public bool Equals(Loglevel other) => string.Equals(_value, other._value, StringComparison.Ordinal);

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override int GetHashCode() => _value?.GetHashCode() ?? 0;

        public override string ToString() => _value;
    }
}
