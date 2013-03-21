
module ParserCombinators.Tests.Json
open NUnit.Framework
open ParserCombinators.Core
open ParserCombinators.Json

[<TestFixture>]
type ``Null parsing``() =
    [<Test>]
    member x.``Empty string should fail``() =
        Assert.AreEqual((Run JsonNullParser ""), (Run (pzero >>% JsonNull) ""))
        
    [<Test>]
    member x.``Parsing "null"``() =
        Assert.AreEqual((Run JsonNullParser "null"), Success(JsonNull, []))
        Assert.AreEqual((Run JsonNullParser "Null"), (Run (pzero >>% JsonNull) ""))




