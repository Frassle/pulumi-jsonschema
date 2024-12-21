module StringTests

open Xunit
open Pulumi.Experimental.Provider

[<Fact>]
let ``Test plain string`` () =
    let t =
        Test.convertSchema
            """{
        "type": "string" 
    }"""

    t.RoundTrip()

    t.ShouldEqual(t.SimpleSchema """{"type":"string"}""")

    PropertyValue("test") |> t.ShouldWrite "\"test\""

    "\"test\"" |> t.ShouldRead(PropertyValue "test")

    let exc = PropertyValue(45) |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"integer\" but should be \"string\""

    let exc = "44" |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"integer\" but should be \"string\""

[<Fact>]
let ``Test string pattern`` () =
    let t =
        Test.convertSchema
            """{
        "type": "string",
        "pattern": "^\\d+$"
    }"""

    t.ShouldEqual(t.SimpleSchema """{"type":"string"}""")

    PropertyValue("123") |> t.ShouldWrite "\"123\""

    "\"456\"" |> t.ShouldRead(PropertyValue "456")

    let exc = PropertyValue("hello") |> t.ShouldThrow<exn>

    exc.Message
    |> Test.shouldEqual "The string value was not a match for the indicated regular expression"

    let exc = "\"bob\"" |> t.ShouldThrow<exn>

    exc.Message
    |> Test.shouldEqual "The string value was not a match for the indicated regular expression"


[<Fact>]
let ``Test string minLength`` () =
    let t =
        Test.convertSchema
            """{
        "type": "string",
        "minLength": 4
    }"""

    t.ShouldEqual(t.SimpleSchema """{"type":"string"}""")

    PropertyValue("1234") |> t.ShouldWrite "\"1234\""

    "\"4567\"" |> t.ShouldRead(PropertyValue "4567")

    let exc = PropertyValue("hi") |> t.ShouldThrow<exn>

    exc.Message
    |> Test.shouldEqual "Value is not longer than or equal to 4 characters"

    let exc = "\"bob\"" |> t.ShouldThrow<exn>

    exc.Message
    |> Test.shouldEqual "Value is not longer than or equal to 4 characters"


[<Fact>]
let ``Test string maxLength`` () =
    let t =
        Test.convertSchema
            """{
        "type": "string",
        "maxLength": 6
    }"""

    t.ShouldEqual(t.SimpleSchema """{"type":"string"}""")

    PropertyValue("hello") |> t.ShouldWrite "\"hello\""

    "\"world\"" |> t.ShouldRead(PropertyValue "world")

    let exc = PropertyValue("goodbye") |> t.ShouldThrow<exn>

    exc.Message
    |> Test.shouldEqual "Value is not shorter than or equal to 6 characters"

    let exc = "\"au revoir\"" |> t.ShouldThrow<exn>

    exc.Message
    |> Test.shouldEqual "Value is not shorter than or equal to 6 characters"
