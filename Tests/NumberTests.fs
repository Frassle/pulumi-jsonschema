module NumberTests

open Xunit
open Pulumi.Experimental.Provider

[<Fact>]
let ``Test number`` () =
    let t =
        Test.convertSchema
            """{
        "type": "number" 
    }"""

    t.RoundTrip()

    t.ShouldEqual(Test.simpleSchema """{"type":"number"}""")

    PropertyValue(14.512) |> t.ShouldWrite "14.512"

    "53.42" |> t.ShouldRead(PropertyValue 53.42)

    let exc = PropertyValue("foo") |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"string\" but should be \"number\""

    let exc = "true" |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"boolean\" but should be \"number\""


[<Fact>]
let ``Test integer`` () =
    let t =
        Test.convertSchema
            """{
        "type": "integer" 
    }"""

    t.RoundTrip()

    t.ShouldEqual(Test.simpleSchema """{"type":"integer"}""")

    PropertyValue(14) |> t.ShouldWrite "14"

    "52" |> t.ShouldRead(PropertyValue 52)

    let exc = PropertyValue(123.56) |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"number\" but should be \"integer\""

    let exc = "123.56" |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"number\" but should be \"integer\""

    let exc = PropertyValue("foo") |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"string\" but should be \"integer\""

    let exc = "true" |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"boolean\" but should be \"integer\""

[<Fact>]
let ``Test integer multipleOf`` () =
    let t =
        Test.convertSchema
            """{
        "type": "integer",
        "multipleOf": 4
    }"""

    t.ShouldEqual(Test.simpleSchema """{"type":"integer"}""")

    PropertyValue(8) |> t.ShouldWrite "8"

    "16" |> t.ShouldRead(PropertyValue 16)

    let exc = "2" |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "2 is not a multiple of 4"

    let exc = PropertyValue(3) |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "3 is not a multiple of 4"
