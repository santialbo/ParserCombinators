
module ParserCombinators.Tests.Json
open NUnit.Framework
open ParserCombinators.Core
open ParserCombinators.Json

[<TestFixture>]
type ``Json null parsing``() =
    [<Test>]
    member x.``Empty string should fail``() =
        Assert.AreEqual((Run (pzero >>% JsonNull) ""), (Run JsonNullParser ""))
        
    [<Test>]
    member x.``Parsing null``() =
        Assert.AreEqual(Success(JsonNull, []), (Run JsonNullParser "null"))
        Assert.AreEqual((Run (pzero >>% JsonNull) ""), (Run JsonNullParser "Null"))
        
[<TestFixture>]
type ``Boolean parsing``() =
    [<Test>]
    member x.``Empty string should fail``() =
        Assert.AreEqual((Run (pzero |>> JsonBoolean) ""), (Run JsonBooleanParser ""))
        
    [<Test>]
    member x.``Parsing "null"``() =
        Assert.AreEqual(Success(JsonBoolean(true), []), (Run JsonBooleanParser "true"))
        Assert.AreEqual(Success(JsonBoolean(false), []), (Run JsonBooleanParser "false"))

[<TestFixture>]
type ``Json key names parsing``() =
    [<Test>]
    member x.``One character names``() =
        Assert.AreEqual(Success("a", []), (Run KeyNameParser "a"))
        Assert.AreEqual(Success("A", []), (Run KeyNameParser "A"))
        Assert.AreEqual(Success("_", []), (Run KeyNameParser "_"))
        Assert.AreEqual((Run (pzero >>% "") ""), (Run KeyNameParser "-"))
        
    [<Test>]
    member x.``Multiple characters names``() =
        Assert.AreEqual(Success("abc", []), (Run KeyNameParser "abc"))
        Assert.AreEqual(Success("ABC", []), (Run KeyNameParser "ABC"))
        Assert.AreEqual(Success("_ABC_DEF", []), (Run KeyNameParser "_ABC_DEF"))
        Assert.AreEqual((Run (pzero >>% "") ""), (Run KeyNameParser "&abc"))
        

[<TestFixture>]
type ``Json escaped string parsing``() =
    [<Test>]
    member x.``String without escaped characters``() =
        Assert.AreEqual(Success(JsonString("abc def\nghi"), []), (Run EscapedStringParser "\"abc def\nghi\""))
        
    [<Test>]
    member x.``String with escaped charactters``() =
        Assert.AreEqual(Success(JsonString("\n"), []), (Run EscapedStringParser @"""\n"""))
        Assert.AreEqual(Success(JsonString("\n\t\r"), []), (Run EscapedStringParser @"""\n\t\r"""))
        Assert.AreEqual(Success(JsonString("abc\ndef\nghi"), []), (Run EscapedStringParser @"""abc\ndef\nghi"""))
        Assert.AreEqual(Success(JsonString("abc \"def\" ghi"), []), (Run EscapedStringParser @"""abc \""def\"" ghi"""))
        Assert.AreEqual(Success(JsonString("\u0400\u0500"), []), (Run EscapedStringParser @"""\u0400\u0500"""))


[<TestFixture>]        
type ``Json key-value pair parsing``() =
    [<Test>]
    member x.``Null value``() =
        Assert.AreEqual(Success(("abc", JsonNull), []), (Run JsonKeyValueParser @"""abc"": null"))
        
    [<Test>]
    member x.``Boolean value``() =
        Assert.AreEqual(Success(("abc", JsonBoolean(true)), []), (Run JsonKeyValueParser @"""abc"": true"))
        Assert.AreEqual(Success(("abc", JsonBoolean(false)), []), (Run JsonKeyValueParser @"""abc"": false"))
        
    [<Test>]
    member x.``Number value``() =
        Assert.AreEqual(Success(("abc", JsonNumber(12.)), []), (Run JsonKeyValueParser @"""abc"": 12"))
        Assert.AreEqual(Success(("abc", JsonNumber(12.e12)), []), (Run JsonKeyValueParser @"""abc"": 12.e12"))
        Assert.AreEqual(Success(("abc", JsonNumber(0.0)), []), (Run JsonKeyValueParser @"""abc"": 0"))
        
    [<Test>]
    member x.``String value``() =
        Assert.AreEqual(Success(("firstName", JsonString("John")), []), (Run JsonKeyValueParser @"""firstName"": ""John"""))


[<TestFixture>]
type ``Json object parsing``() =
    [<Test>]
    member x.``Parsing empty object``() =
        Assert.AreEqual(Success(JsonObject(Map.empty), []), (Run JsonParser "{}"))
        
    [<Test>]
    member x.``Parsing object with 1 key-value pair``() =
        Assert.AreEqual(Success(JsonObject(Map.ofList ["firstName", JsonString("John")]), []), (Run JsonParser """{"firstName": "John" }"""))
    
    [<Test>]
    member x.``Parsing complex objects``() =
        let text = """{
            "firstName": "John",
            "lastName": "Smith",
            "age": 25,
            "address": {
                "streetAddress": "21 2nd Street",
                "city": "New York",
                "state": "NY",
                "postalCode": 10021
            },
            "phoneNumber": [
                {
                    "type": "home",
                    "number": "212 555-1234"
                },
                {
                    "type": "fax",
                    "number": "646 555-4567"
                }
            ]
        }"""
        let expected =
            JsonObject(
                Map.ofList [
                    ("address", JsonObject (
                        Map.ofList [
                            ("city", JsonString "New York");
                            ("postalCode", JsonNumber 10021.0);
                            ("state", JsonString "NY");
                            ("streetAddress", JsonString "21 2nd Street")]));
                    ("age", JsonNumber 25.0);
                    ("firstName", JsonString "John");
                    ("lastName", JsonString "Smith");
                    ("phoneNumber", JsonArray (
                        [
                            JsonObject (
                                Map.ofList [
                                    ("number", JsonString "212 555-1234");
                                    ("type", JsonString "home")]);
                            JsonObject (
                                Map.ofList [
                                    ("number", JsonString "646 555-4567");
                                    ("type", JsonString "fax")])]))])
        Assert.AreEqual(Success(expected, []), (Run JsonParser text))
    
    [<Test>]
    member x.``Parsing json array``() =
        let text = """[1, 2, 3, 4]"""
        let expected = JsonArray [JsonNumber 1.; JsonNumber 2.; JsonNumber 3.; JsonNumber 4.]
        Assert.AreEqual(Success(expected, []), (Run JsonParser text))                       
