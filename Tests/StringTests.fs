module StringTests

open Xunit

[<Fact>]
let ``Test plain string`` () =
    let t = Test.convertSchema """{
        "type": "string" 
    }"""
    t.RoundTrip()
    
    t.ShouldEqual (Test.simpleSchema """{"type":"string"}""")

    Pulumi.Provider.PropertyValue("test")
    |> t.ShouldWrite "\"test\""

    "\"test\""
    |> t.ShouldRead (Pulumi.Provider.PropertyValue "test")
    
    let exc = 
        Pulumi.Provider.PropertyValue(45)
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"integer\" but should be \"string\""

    let exc = 
        "44"
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is \"integer\" but should be \"string\""

[<Fact>]
let ``Test string pattern`` () =
    let t = Test.convertSchema """{
        "type": "string",
        "pattern": "^\\d+$"
    }"""
    
    t.ShouldEqual (Test.simpleSchema """{"type":"string"}""")

    Pulumi.Provider.PropertyValue("123")
    |> t.ShouldWrite "\"123\""

    "\"456\""
    |> t.ShouldRead (Pulumi.Provider.PropertyValue "456")

    let exc = 
        Pulumi.Provider.PropertyValue("hello")
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "The string value was not a match for the indicated regular expression"
    
    let exc = 
        "\"bob\""
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "The string value was not a match for the indicated regular expression"

    
[<Fact>]
let ``Test string minLength`` () =
    let t = Test.convertSchema """{
        "type": "string",
        "minLength": 4
    }"""
    
    t.ShouldEqual (Test.simpleSchema """{"type":"string"}""")

    Pulumi.Provider.PropertyValue("1234")
    |> t.ShouldWrite "\"1234\""

    "\"4567\""
    |> t.ShouldRead (Pulumi.Provider.PropertyValue "4567")

    let exc = 
        Pulumi.Provider.PropertyValue("hi")
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is not longer than or equal to 4 characters"
    
    let exc = 
        "\"bob\""
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is not longer than or equal to 4 characters"

    
[<Fact>]
let ``Test string maxLength`` () =
    let t = Test.convertSchema """{
        "type": "string",
        "maxLength": 6
    }"""
    
    t.ShouldEqual (Test.simpleSchema """{"type":"string"}""")

    Pulumi.Provider.PropertyValue("hello")
    |> t.ShouldWrite "\"hello\""

    "\"world\""
    |> t.ShouldRead (Pulumi.Provider.PropertyValue "world")

    let exc = 
        Pulumi.Provider.PropertyValue("goodbye")
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is not shorter than or equal to 6 characters"
    
    let exc = 
        "\"au revoir\""
        |> t.ShouldThrow<exn>
    exc.Message |> Test.shouldEqual "Value is not shorter than or equal to 6 characters"