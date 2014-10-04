namespace TypeProviderTest

open FSharp.Data
open System.IO
open NUnit.Framework

type WorldBank = JsonProvider<"resources/bank.json">

[<TestFixture>]
type JsonFileProviderTest() =

    [<Test>]
    member this.RecordTest() =
        let bank = WorldBank.GetSample()

        Assert.AreEqual(1, bank.Record.Page)
        Assert.AreEqual(1, bank.Record.Pages)

    [<Test>]
    member this.ArrayTest() =
        let bank = WorldBank.GetSample()
        Assert.AreEqual(bank.Array.Length, 2)
