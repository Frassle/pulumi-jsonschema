// *** WARNING: this file was generated by pulumi. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

using System;
using System.ComponentModel;
using Pulumi;

namespace Pulumi.Pulumi
{
    /// <summary>
    /// The primitive type, if any
    /// </summary>
    [EnumType]
    public readonly struct AdditionalPropertiesChoice1Of5Type : IEquatable<AdditionalPropertiesChoice1Of5Type>
    {
        private readonly string _value;

        private AdditionalPropertiesChoice1Of5Type(string value)
        {
            _value = value ?? throw new ArgumentNullException(nameof(value));
        }

        public static AdditionalPropertiesChoice1Of5Type Boolean { get; } = new AdditionalPropertiesChoice1Of5Type("boolean");
        public static AdditionalPropertiesChoice1Of5Type Integer { get; } = new AdditionalPropertiesChoice1Of5Type("integer");
        public static AdditionalPropertiesChoice1Of5Type Number { get; } = new AdditionalPropertiesChoice1Of5Type("number");
        public static AdditionalPropertiesChoice1Of5Type @String { get; } = new AdditionalPropertiesChoice1Of5Type("string");

        public static bool operator ==(AdditionalPropertiesChoice1Of5Type left, AdditionalPropertiesChoice1Of5Type right) => left.Equals(right);
        public static bool operator !=(AdditionalPropertiesChoice1Of5Type left, AdditionalPropertiesChoice1Of5Type right) => !left.Equals(right);

        public static explicit operator string(AdditionalPropertiesChoice1Of5Type value) => value._value;

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override bool Equals(object? obj) => obj is AdditionalPropertiesChoice1Of5Type other && Equals(other);
        public bool Equals(AdditionalPropertiesChoice1Of5Type other) => string.Equals(_value, other._value, StringComparison.Ordinal);

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override int GetHashCode() => _value?.GetHashCode() ?? 0;

        public override string ToString() => _value;
    }

    /// <summary>
    /// The underlying primitive type of the union, if any
    /// </summary>
    [EnumType]
    public readonly struct AdditionalPropertiesChoice5Of5Type : IEquatable<AdditionalPropertiesChoice5Of5Type>
    {
        private readonly string _value;

        private AdditionalPropertiesChoice5Of5Type(string value)
        {
            _value = value ?? throw new ArgumentNullException(nameof(value));
        }

        public static AdditionalPropertiesChoice5Of5Type Boolean { get; } = new AdditionalPropertiesChoice5Of5Type("boolean");
        public static AdditionalPropertiesChoice5Of5Type Integer { get; } = new AdditionalPropertiesChoice5Of5Type("integer");
        public static AdditionalPropertiesChoice5Of5Type Number { get; } = new AdditionalPropertiesChoice5Of5Type("number");
        public static AdditionalPropertiesChoice5Of5Type @String { get; } = new AdditionalPropertiesChoice5Of5Type("string");

        public static bool operator ==(AdditionalPropertiesChoice5Of5Type left, AdditionalPropertiesChoice5Of5Type right) => left.Equals(right);
        public static bool operator !=(AdditionalPropertiesChoice5Of5Type left, AdditionalPropertiesChoice5Of5Type right) => !left.Equals(right);

        public static explicit operator string(AdditionalPropertiesChoice5Of5Type value) => value._value;

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override bool Equals(object? obj) => obj is AdditionalPropertiesChoice5Of5Type other && Equals(other);
        public bool Equals(AdditionalPropertiesChoice5Of5Type other) => string.Equals(_value, other._value, StringComparison.Ordinal);

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override int GetHashCode() => _value?.GetHashCode() ?? 0;

        public override string ToString() => _value;
    }

    /// <summary>
    /// The primitive type, if any
    /// </summary>
    [EnumType]
    public readonly struct Choice1Of5Type : IEquatable<Choice1Of5Type>
    {
        private readonly string _value;

        private Choice1Of5Type(string value)
        {
            _value = value ?? throw new ArgumentNullException(nameof(value));
        }

        public static Choice1Of5Type Boolean { get; } = new Choice1Of5Type("boolean");
        public static Choice1Of5Type Integer { get; } = new Choice1Of5Type("integer");
        public static Choice1Of5Type Number { get; } = new Choice1Of5Type("number");
        public static Choice1Of5Type @String { get; } = new Choice1Of5Type("string");

        public static bool operator ==(Choice1Of5Type left, Choice1Of5Type right) => left.Equals(right);
        public static bool operator !=(Choice1Of5Type left, Choice1Of5Type right) => !left.Equals(right);

        public static explicit operator string(Choice1Of5Type value) => value._value;

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override bool Equals(object? obj) => obj is Choice1Of5Type other && Equals(other);
        public bool Equals(Choice1Of5Type other) => string.Equals(_value, other._value, StringComparison.Ordinal);

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override int GetHashCode() => _value?.GetHashCode() ?? 0;

        public override string ToString() => _value;
    }

    /// <summary>
    /// The underlying primitive type of the union, if any
    /// </summary>
    [EnumType]
    public readonly struct Choice5Of5Type : IEquatable<Choice5Of5Type>
    {
        private readonly string _value;

        private Choice5Of5Type(string value)
        {
            _value = value ?? throw new ArgumentNullException(nameof(value));
        }

        public static Choice5Of5Type Boolean { get; } = new Choice5Of5Type("boolean");
        public static Choice5Of5Type Integer { get; } = new Choice5Of5Type("integer");
        public static Choice5Of5Type Number { get; } = new Choice5Of5Type("number");
        public static Choice5Of5Type @String { get; } = new Choice5Of5Type("string");

        public static bool operator ==(Choice5Of5Type left, Choice5Of5Type right) => left.Equals(right);
        public static bool operator !=(Choice5Of5Type left, Choice5Of5Type right) => !left.Equals(right);

        public static explicit operator string(Choice5Of5Type value) => value._value;

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override bool Equals(object? obj) => obj is Choice5Of5Type other && Equals(other);
        public bool Equals(Choice5Of5Type other) => string.Equals(_value, other._value, StringComparison.Ordinal);

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override int GetHashCode() => _value?.GetHashCode() ?? 0;

        public override string ToString() => _value;
    }

    /// <summary>
    /// The underlying primitive type of the enum
    /// </summary>
    [EnumType]
    public readonly struct EnumTypeSpecPropertiesType : IEquatable<EnumTypeSpecPropertiesType>
    {
        private readonly string _value;

        private EnumTypeSpecPropertiesType(string value)
        {
            _value = value ?? throw new ArgumentNullException(nameof(value));
        }

        public static EnumTypeSpecPropertiesType Boolean { get; } = new EnumTypeSpecPropertiesType("boolean");
        public static EnumTypeSpecPropertiesType Integer { get; } = new EnumTypeSpecPropertiesType("integer");
        public static EnumTypeSpecPropertiesType Number { get; } = new EnumTypeSpecPropertiesType("number");
        public static EnumTypeSpecPropertiesType @String { get; } = new EnumTypeSpecPropertiesType("string");

        public static bool operator ==(EnumTypeSpecPropertiesType left, EnumTypeSpecPropertiesType right) => left.Equals(right);
        public static bool operator !=(EnumTypeSpecPropertiesType left, EnumTypeSpecPropertiesType right) => !left.Equals(right);

        public static explicit operator string(EnumTypeSpecPropertiesType value) => value._value;

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override bool Equals(object? obj) => obj is EnumTypeSpecPropertiesType other && Equals(other);
        public bool Equals(EnumTypeSpecPropertiesType other) => string.Equals(_value, other._value, StringComparison.Ordinal);

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override int GetHashCode() => _value?.GetHashCode() ?? 0;

        public override string ToString() => _value;
    }

    /// <summary>
    /// The underlying primitive type of the union, if any
    /// </summary>
    [EnumType]
    public readonly struct InputPropertiesAdditionalPropertiesChoice5Of5Type : IEquatable<InputPropertiesAdditionalPropertiesChoice5Of5Type>
    {
        private readonly string _value;

        private InputPropertiesAdditionalPropertiesChoice5Of5Type(string value)
        {
            _value = value ?? throw new ArgumentNullException(nameof(value));
        }

        public static InputPropertiesAdditionalPropertiesChoice5Of5Type Boolean { get; } = new InputPropertiesAdditionalPropertiesChoice5Of5Type("boolean");
        public static InputPropertiesAdditionalPropertiesChoice5Of5Type Integer { get; } = new InputPropertiesAdditionalPropertiesChoice5Of5Type("integer");
        public static InputPropertiesAdditionalPropertiesChoice5Of5Type Number { get; } = new InputPropertiesAdditionalPropertiesChoice5Of5Type("number");
        public static InputPropertiesAdditionalPropertiesChoice5Of5Type @String { get; } = new InputPropertiesAdditionalPropertiesChoice5Of5Type("string");

        public static bool operator ==(InputPropertiesAdditionalPropertiesChoice5Of5Type left, InputPropertiesAdditionalPropertiesChoice5Of5Type right) => left.Equals(right);
        public static bool operator !=(InputPropertiesAdditionalPropertiesChoice5Of5Type left, InputPropertiesAdditionalPropertiesChoice5Of5Type right) => !left.Equals(right);

        public static explicit operator string(InputPropertiesAdditionalPropertiesChoice5Of5Type value) => value._value;

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override bool Equals(object? obj) => obj is InputPropertiesAdditionalPropertiesChoice5Of5Type other && Equals(other);
        public bool Equals(InputPropertiesAdditionalPropertiesChoice5Of5Type other) => string.Equals(_value, other._value, StringComparison.Ordinal);

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override int GetHashCode() => _value?.GetHashCode() ?? 0;

        public override string ToString() => _value;
    }

    /// <summary>
    /// The primitive type, if any
    /// </summary>
    [EnumType]
    public readonly struct OneOf0PropertiesType : IEquatable<OneOf0PropertiesType>
    {
        private readonly string _value;

        private OneOf0PropertiesType(string value)
        {
            _value = value ?? throw new ArgumentNullException(nameof(value));
        }

        public static OneOf0PropertiesType Boolean { get; } = new OneOf0PropertiesType("boolean");
        public static OneOf0PropertiesType Integer { get; } = new OneOf0PropertiesType("integer");
        public static OneOf0PropertiesType Number { get; } = new OneOf0PropertiesType("number");
        public static OneOf0PropertiesType @String { get; } = new OneOf0PropertiesType("string");

        public static bool operator ==(OneOf0PropertiesType left, OneOf0PropertiesType right) => left.Equals(right);
        public static bool operator !=(OneOf0PropertiesType left, OneOf0PropertiesType right) => !left.Equals(right);

        public static explicit operator string(OneOf0PropertiesType value) => value._value;

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override bool Equals(object? obj) => obj is OneOf0PropertiesType other && Equals(other);
        public bool Equals(OneOf0PropertiesType other) => string.Equals(_value, other._value, StringComparison.Ordinal);

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override int GetHashCode() => _value?.GetHashCode() ?? 0;

        public override string ToString() => _value;
    }

    /// <summary>
    /// The underlying primitive type of the union, if any
    /// </summary>
    [EnumType]
    public readonly struct OneOf4PropertiesType : IEquatable<OneOf4PropertiesType>
    {
        private readonly string _value;

        private OneOf4PropertiesType(string value)
        {
            _value = value ?? throw new ArgumentNullException(nameof(value));
        }

        public static OneOf4PropertiesType Boolean { get; } = new OneOf4PropertiesType("boolean");
        public static OneOf4PropertiesType Integer { get; } = new OneOf4PropertiesType("integer");
        public static OneOf4PropertiesType Number { get; } = new OneOf4PropertiesType("number");
        public static OneOf4PropertiesType @String { get; } = new OneOf4PropertiesType("string");

        public static bool operator ==(OneOf4PropertiesType left, OneOf4PropertiesType right) => left.Equals(right);
        public static bool operator !=(OneOf4PropertiesType left, OneOf4PropertiesType right) => !left.Equals(right);

        public static explicit operator string(OneOf4PropertiesType value) => value._value;

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override bool Equals(object? obj) => obj is OneOf4PropertiesType other && Equals(other);
        public bool Equals(OneOf4PropertiesType other) => string.Equals(_value, other._value, StringComparison.Ordinal);

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override int GetHashCode() => _value?.GetHashCode() ?? 0;

        public override string ToString() => _value;
    }

    /// <summary>
    /// The primitive type, if any
    /// </summary>
    [EnumType]
    public readonly struct PropertiesAdditionalPropertiesChoice1Of5Type : IEquatable<PropertiesAdditionalPropertiesChoice1Of5Type>
    {
        private readonly string _value;

        private PropertiesAdditionalPropertiesChoice1Of5Type(string value)
        {
            _value = value ?? throw new ArgumentNullException(nameof(value));
        }

        public static PropertiesAdditionalPropertiesChoice1Of5Type Boolean { get; } = new PropertiesAdditionalPropertiesChoice1Of5Type("boolean");
        public static PropertiesAdditionalPropertiesChoice1Of5Type Integer { get; } = new PropertiesAdditionalPropertiesChoice1Of5Type("integer");
        public static PropertiesAdditionalPropertiesChoice1Of5Type Number { get; } = new PropertiesAdditionalPropertiesChoice1Of5Type("number");
        public static PropertiesAdditionalPropertiesChoice1Of5Type @String { get; } = new PropertiesAdditionalPropertiesChoice1Of5Type("string");

        public static bool operator ==(PropertiesAdditionalPropertiesChoice1Of5Type left, PropertiesAdditionalPropertiesChoice1Of5Type right) => left.Equals(right);
        public static bool operator !=(PropertiesAdditionalPropertiesChoice1Of5Type left, PropertiesAdditionalPropertiesChoice1Of5Type right) => !left.Equals(right);

        public static explicit operator string(PropertiesAdditionalPropertiesChoice1Of5Type value) => value._value;

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override bool Equals(object? obj) => obj is PropertiesAdditionalPropertiesChoice1Of5Type other && Equals(other);
        public bool Equals(PropertiesAdditionalPropertiesChoice1Of5Type other) => string.Equals(_value, other._value, StringComparison.Ordinal);

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override int GetHashCode() => _value?.GetHashCode() ?? 0;

        public override string ToString() => _value;
    }

    /// <summary>
    /// The underlying primitive type of the union, if any
    /// </summary>
    [EnumType]
    public readonly struct PropertiesAdditionalPropertiesChoice5Of5Type : IEquatable<PropertiesAdditionalPropertiesChoice5Of5Type>
    {
        private readonly string _value;

        private PropertiesAdditionalPropertiesChoice5Of5Type(string value)
        {
            _value = value ?? throw new ArgumentNullException(nameof(value));
        }

        public static PropertiesAdditionalPropertiesChoice5Of5Type Boolean { get; } = new PropertiesAdditionalPropertiesChoice5Of5Type("boolean");
        public static PropertiesAdditionalPropertiesChoice5Of5Type Integer { get; } = new PropertiesAdditionalPropertiesChoice5Of5Type("integer");
        public static PropertiesAdditionalPropertiesChoice5Of5Type Number { get; } = new PropertiesAdditionalPropertiesChoice5Of5Type("number");
        public static PropertiesAdditionalPropertiesChoice5Of5Type @String { get; } = new PropertiesAdditionalPropertiesChoice5Of5Type("string");

        public static bool operator ==(PropertiesAdditionalPropertiesChoice5Of5Type left, PropertiesAdditionalPropertiesChoice5Of5Type right) => left.Equals(right);
        public static bool operator !=(PropertiesAdditionalPropertiesChoice5Of5Type left, PropertiesAdditionalPropertiesChoice5Of5Type right) => !left.Equals(right);

        public static explicit operator string(PropertiesAdditionalPropertiesChoice5Of5Type value) => value._value;

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override bool Equals(object? obj) => obj is PropertiesAdditionalPropertiesChoice5Of5Type other && Equals(other);
        public bool Equals(PropertiesAdditionalPropertiesChoice5Of5Type other) => string.Equals(_value, other._value, StringComparison.Ordinal);

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override int GetHashCode() => _value?.GetHashCode() ?? 0;

        public override string ToString() => _value;
    }

    /// <summary>
    /// The primitive type, if any
    /// </summary>
    [EnumType]
    public readonly struct PropertiesType : IEquatable<PropertiesType>
    {
        private readonly string _value;

        private PropertiesType(string value)
        {
            _value = value ?? throw new ArgumentNullException(nameof(value));
        }

        public static PropertiesType Boolean { get; } = new PropertiesType("boolean");
        public static PropertiesType Integer { get; } = new PropertiesType("integer");
        public static PropertiesType Number { get; } = new PropertiesType("number");
        public static PropertiesType @String { get; } = new PropertiesType("string");

        public static bool operator ==(PropertiesType left, PropertiesType right) => left.Equals(right);
        public static bool operator !=(PropertiesType left, PropertiesType right) => !left.Equals(right);

        public static explicit operator string(PropertiesType value) => value._value;

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override bool Equals(object? obj) => obj is PropertiesType other && Equals(other);
        public bool Equals(PropertiesType other) => string.Equals(_value, other._value, StringComparison.Ordinal);

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override int GetHashCode() => _value?.GetHashCode() ?? 0;

        public override string ToString() => _value;
    }

    /// <summary>
    /// The primitive type, if any
    /// </summary>
    [EnumType]
    public readonly struct Type : IEquatable<Type>
    {
        private readonly string _value;

        private Type(string value)
        {
            _value = value ?? throw new ArgumentNullException(nameof(value));
        }

        public static Type Boolean { get; } = new Type("boolean");
        public static Type Integer { get; } = new Type("integer");
        public static Type Number { get; } = new Type("number");
        public static Type @String { get; } = new Type("string");

        public static bool operator ==(Type left, Type right) => left.Equals(right);
        public static bool operator !=(Type left, Type right) => !left.Equals(right);

        public static explicit operator string(Type value) => value._value;

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override bool Equals(object? obj) => obj is Type other && Equals(other);
        public bool Equals(Type other) => string.Equals(_value, other._value, StringComparison.Ordinal);

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override int GetHashCode() => _value?.GetHashCode() ?? 0;

        public override string ToString() => _value;
    }

    /// <summary>
    /// The primitive type, if any
    /// </summary>
    [EnumType]
    public readonly struct TypeSpecOneOf0PropertiesType : IEquatable<TypeSpecOneOf0PropertiesType>
    {
        private readonly string _value;

        private TypeSpecOneOf0PropertiesType(string value)
        {
            _value = value ?? throw new ArgumentNullException(nameof(value));
        }

        public static TypeSpecOneOf0PropertiesType Boolean { get; } = new TypeSpecOneOf0PropertiesType("boolean");
        public static TypeSpecOneOf0PropertiesType Integer { get; } = new TypeSpecOneOf0PropertiesType("integer");
        public static TypeSpecOneOf0PropertiesType Number { get; } = new TypeSpecOneOf0PropertiesType("number");
        public static TypeSpecOneOf0PropertiesType @String { get; } = new TypeSpecOneOf0PropertiesType("string");

        public static bool operator ==(TypeSpecOneOf0PropertiesType left, TypeSpecOneOf0PropertiesType right) => left.Equals(right);
        public static bool operator !=(TypeSpecOneOf0PropertiesType left, TypeSpecOneOf0PropertiesType right) => !left.Equals(right);

        public static explicit operator string(TypeSpecOneOf0PropertiesType value) => value._value;

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override bool Equals(object? obj) => obj is TypeSpecOneOf0PropertiesType other && Equals(other);
        public bool Equals(TypeSpecOneOf0PropertiesType other) => string.Equals(_value, other._value, StringComparison.Ordinal);

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override int GetHashCode() => _value?.GetHashCode() ?? 0;

        public override string ToString() => _value;
    }

    /// <summary>
    /// The underlying primitive type of the union, if any
    /// </summary>
    [EnumType]
    public readonly struct TypeSpecOneOf4PropertiesType : IEquatable<TypeSpecOneOf4PropertiesType>
    {
        private readonly string _value;

        private TypeSpecOneOf4PropertiesType(string value)
        {
            _value = value ?? throw new ArgumentNullException(nameof(value));
        }

        public static TypeSpecOneOf4PropertiesType Boolean { get; } = new TypeSpecOneOf4PropertiesType("boolean");
        public static TypeSpecOneOf4PropertiesType Integer { get; } = new TypeSpecOneOf4PropertiesType("integer");
        public static TypeSpecOneOf4PropertiesType Number { get; } = new TypeSpecOneOf4PropertiesType("number");
        public static TypeSpecOneOf4PropertiesType @String { get; } = new TypeSpecOneOf4PropertiesType("string");

        public static bool operator ==(TypeSpecOneOf4PropertiesType left, TypeSpecOneOf4PropertiesType right) => left.Equals(right);
        public static bool operator !=(TypeSpecOneOf4PropertiesType left, TypeSpecOneOf4PropertiesType right) => !left.Equals(right);

        public static explicit operator string(TypeSpecOneOf4PropertiesType value) => value._value;

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override bool Equals(object? obj) => obj is TypeSpecOneOf4PropertiesType other && Equals(other);
        public bool Equals(TypeSpecOneOf4PropertiesType other) => string.Equals(_value, other._value, StringComparison.Ordinal);

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override int GetHashCode() => _value?.GetHashCode() ?? 0;

        public override string ToString() => _value;
    }

    /// <summary>
    /// The underlying primitive type of the union, if any
    /// </summary>
    [EnumType]
    public readonly struct VariablesAdditionalPropertiesChoice5Of5Type : IEquatable<VariablesAdditionalPropertiesChoice5Of5Type>
    {
        private readonly string _value;

        private VariablesAdditionalPropertiesChoice5Of5Type(string value)
        {
            _value = value ?? throw new ArgumentNullException(nameof(value));
        }

        public static VariablesAdditionalPropertiesChoice5Of5Type Boolean { get; } = new VariablesAdditionalPropertiesChoice5Of5Type("boolean");
        public static VariablesAdditionalPropertiesChoice5Of5Type Integer { get; } = new VariablesAdditionalPropertiesChoice5Of5Type("integer");
        public static VariablesAdditionalPropertiesChoice5Of5Type Number { get; } = new VariablesAdditionalPropertiesChoice5Of5Type("number");
        public static VariablesAdditionalPropertiesChoice5Of5Type @String { get; } = new VariablesAdditionalPropertiesChoice5Of5Type("string");

        public static bool operator ==(VariablesAdditionalPropertiesChoice5Of5Type left, VariablesAdditionalPropertiesChoice5Of5Type right) => left.Equals(right);
        public static bool operator !=(VariablesAdditionalPropertiesChoice5Of5Type left, VariablesAdditionalPropertiesChoice5Of5Type right) => !left.Equals(right);

        public static explicit operator string(VariablesAdditionalPropertiesChoice5Of5Type value) => value._value;

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override bool Equals(object? obj) => obj is VariablesAdditionalPropertiesChoice5Of5Type other && Equals(other);
        public bool Equals(VariablesAdditionalPropertiesChoice5Of5Type other) => string.Equals(_value, other._value, StringComparison.Ordinal);

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override int GetHashCode() => _value?.GetHashCode() ?? 0;

        public override string ToString() => _value;
    }
}
