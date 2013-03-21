
module ParserCombinators.Tests.Core
open NUnit.Framework
open ParserCombinators.Core

[<TestFixture>]
type ``Integer parsing``() =
    [<Test>]
    member x.``Empty string should fail``() =
        Assert.AreEqual((Run (pzero |>> int) ""), (Run IntegerParser ""))
        
    [<Test>]
    member x.``One digit integers``() =
        Assert.AreEqual(Success(0, []), (Run IntegerParser "0"))
        Assert.AreEqual(Success(1, []), (Run IntegerParser "1"))
        
    [<Test>]
    member x.``Multiple digit integers``() =
        Assert.AreEqual(Success(123, []), (Run IntegerParser "0123"))
        Assert.AreEqual(Success(123, []), (Run IntegerParser "123"))
        
    [<Test>]
    member x.``Integer with signs``() =
        Assert.AreEqual(Success(123, []), (Run IntegerParser "+0123"))
        Assert.AreEqual(Success(123, []), (Run IntegerParser "+123"))
        Assert.AreEqual(Success(-123, []), (Run IntegerParser "-0123"))
        Assert.AreEqual(Success(-123, []), (Run IntegerParser "-123"))
    
    
[<TestFixture>]
type ``Float parsing``() =
    [<Test>]
    member x.``Parsing empty string should fail``() =
        Assert.AreSame((Run (pzero |>> float) ""), (Run FloatParser ""))
        
    [<Test>]
    member x.``Parsing zero``() =
        Assert.AreEqual(Success(0.0, []), (Run FloatParser "0"))
        Assert.AreEqual(Success(0.0, []), (Run FloatParser ".0"))
        Assert.AreEqual(Success(0.0, []), (Run FloatParser "-0"))
        Assert.AreEqual(Success(0.0, []), (Run FloatParser "+0"))
        Assert.AreEqual(Success(0.0, []), (Run FloatParser "-0.0"))
        
    [<Test>]
    member x.``Parsing integers``() =
        Assert.AreEqual(Success(123.0, []), (Run FloatParser "123"))
        Assert.AreEqual(Success(123.0, []), (Run FloatParser "+123"))
        Assert.AreEqual(Success(-123.0, []), (Run FloatParser "-123"))
        
    [<Test>]
    member x.``Floats without leading digits``() =
        Assert.AreEqual(Success(0.012, []), (Run FloatParser ".012"))
        Assert.AreEqual(Success(0.0, []), (Run FloatParser ".0"))
        Assert.AreEqual(Success(1.0, []), (Run FloatParser "1."))
        Assert.AreEqual(Success(0.012, []), (Run FloatParser "+.012"))
        Assert.AreEqual(Success(-0.012, []), (Run FloatParser "-.012"))
        
    [<Test>]
    member x.``Floats in scientific notation``() =
        Assert.AreEqual(Success(1.23e0, []), (Run FloatParser "1.23e0"))
        Assert.AreEqual(Success(0.0, []), (Run FloatParser ".0e12"))
        Assert.AreEqual(Success(0.012e-12, []), (Run FloatParser "+.012e-12"))
        Assert.AreEqual(Success(-0.012e1, []), (Run FloatParser "-.012E+1"))
        

[<TestFixture>]
type ``Escaped characters parsing``() =
    [<Test>]
    member x.``Utf16 character``() =
        Assert.AreEqual(Success('@', []), (Run EscapedUtf16CharParser @"\u0040"))
        Assert.AreEqual(Success('\u1234', []), (Run EscapedUtf16CharParser @"\u1234"))
        Assert.AreEqual(Success('\u1fff', []), (Run EscapedUtf16CharParser @"\U1fff"))
        Assert.AreEqual(Success('\u2ABC', []), (Run EscapedUtf16CharParser @"\U2abc"))
        
    [<Test>]
    member x.``Other characters``() =
        Assert.AreEqual(Success('\n', []), (Run EscapedCharParser @"\n"))
        Assert.AreEqual(Success('\r', []), (Run EscapedCharParser @"\r"))
        Assert.AreEqual(Success('\t', []), (Run EscapedCharParser @"\t"))
        Assert.AreEqual(Success('\\', []), (Run EscapedCharParser @"\\"))
