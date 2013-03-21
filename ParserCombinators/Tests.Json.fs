
module ParserCombinators.Tests.Json
open NUnit.Framework
open ParserCombinators.Core
open ParserCombinators.Json

[<TestFixture>]
type ``Null parsing``() =
    [<Test>]
    member x.``Empty string should fail``() =
        Assert.AreEqual((Run NullParser ""), (Run (pzero >>% Null) ""))
        
    [<Test>]
    member x.``Parsing "null"``() =
        Assert.AreEqual((Run NullParser "null"), Success(Null, []))
        Assert.AreEqual((Run NullParser "Null"), (Run (pzero >>% Null) ""))