
namespace TypeProviderTest

open FSharp.Data
open NUnit.Framework

type Simple = JsonProvider<""" { "name": "wk", "age": 20 } """>

[<TestFixture>]
type SimpleJsonProviderTest() =

    [<Test>]
    member this.SimpleTest() =
        let simple = Simple.Parse(""" { "name": "kw", "age": 30 } """)
        Assert.AreEqual(30, simple.Age)
        Assert.AreEqual("kw", simple.Name);