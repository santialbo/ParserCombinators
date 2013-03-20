
module ParserCombinators.Tests
open NUnit.Framework
open ParserCombinators.Core

[<TestFixture>]
type ``Integer parsing``() =
    [<Test>]
    member x.``Empty string should fail``() =
        Assert.AreEqual((Run IntegerParser ""), (Run (pzero |>> int) ""))
        
    [<Test>]
    member x.``One digit integers``() =
        Assert.AreEqual((Run IntegerParser "0"), Success(0, []))
        Assert.AreEqual((Run IntegerParser "1"), Success(1, []))
        
    [<Test>]
    member x.``Multiple digit integers``() =
        Assert.AreEqual((Run IntegerParser "0123"), Success(123, []))
        Assert.AreEqual((Run IntegerParser "123"), Success(123, []))
        
    [<Test>]
    member x.``Integer with signs``() =
        Assert.AreEqual((Run IntegerParser "+0123"), Success(123, []))
        Assert.AreEqual((Run IntegerParser "+123"), Success(123, []))
        Assert.AreEqual((Run IntegerParser "-0123"), Success(-123, []))
        Assert.AreEqual((Run IntegerParser "-123"), Success(-123, []))
    
    
[<TestFixture>]
type ``Float parsing``() =
    [<Test>]
    member x.``Parsing zero``() =
        Assert.AreEqual((Run FloatParser "0"), Success(0.0, []))
        Assert.AreEqual((Run FloatParser ".0"), Success(0.0, []))
        Assert.AreEqual((Run FloatParser "-0"), Success(0.0, []))
        Assert.AreEqual((Run FloatParser "+0"), Success(0.0, []))
        Assert.AreEqual((Run FloatParser "-0.0"), Success(0.0, []))
        
    [<Test>]
    member x.``Parsing integers``() =
        Assert.AreEqual((Run FloatParser "123"), Success(123.0, []))
        Assert.AreEqual((Run FloatParser "+123"), Success(123.0, []))
        Assert.AreEqual((Run FloatParser "-123"), Success(-123.0, []))
        
    [<Test>]
    member x.``Floats without leading digits``() =
        Assert.AreEqual((Run FloatParser ".012"), Success(0.012, []))
        Assert.AreEqual((Run FloatParser ".0"), Success(0.0, []))
        Assert.AreEqual((Run FloatParser "+.012"), Success(0.012, []))
        Assert.AreEqual((Run FloatParser "-.012"), Success(-0.012, []))
        
    [<Test>]
    member x.``Floats in scientific notation``() =
        Assert.AreEqual((Run FloatParser "1.23e0"), Success(1.23e0, []))
        Assert.AreEqual((Run FloatParser ".0e12"), Success(0.0, []))
        Assert.AreEqual((Run FloatParser "+.012e-12"), Success(0.012e-12, []))
        Assert.AreEqual((Run FloatParser "-.012E+1"), Success(-0.012e1, []))
        

open ParserCombinators.Json

[<TestFixture>]
type ``Json key names``() =
    [<Test>]
    member x.``Empty string should fail``() =
        Assert.AreEqual((Run KeyNameParser ""), (Run (pzero |>> string) ""))
    
    [<Test>]
    member x.``One character names``() =
        Assert.AreEqual((Run KeyNameParser "1"), (Run (pzero |>> string) ""))
        Assert.AreEqual((Run KeyNameParser "-"), (Run (pzero |>> string) ""))
        Assert.AreEqual((Run KeyNameParser "_"), Success("_", []))
        Assert.AreEqual((Run KeyNameParser "a"), Success("a", []))
    
    [<Test>]
    member x.``Valid key names``() =
        Assert.AreEqual((Run KeyNameParser "_abc"), Success("_abc", []))
        Assert.AreEqual((Run KeyNameParser "_abc123def456"), Success("_abc123def456", []))
        Assert.AreEqual((Run KeyNameParser "abc123def456"), Success("abc123def456", []))
        Assert.AreEqual((Run KeyNameParser "abc-def"), Success("abc",['-'; 'd'; 'e'; 'f']))
