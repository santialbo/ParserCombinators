
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
    member x.``Parsing "null"``() =
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

[<TestFixture>]
type ``Json object parsing``() =
    [<Test>]
    member x.``Parsing empty object``() =
        Assert.AreEqual(Success(JsonObject(Map.empty), []), (Run JsonObjectParser "{}"))
    member x.``Parsing "null"``() =
        Assert.AreEqual((Run JsonNullParser "null"), Success(JsonNull, []))
        Assert.AreEqual((Run JsonNullParser "Null"), (Run (pzero >>% JsonNull) ""))
        
