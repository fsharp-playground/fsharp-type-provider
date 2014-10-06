
namespace TypeProviderTest

open FSharp.Data
open NUnit.Framework
open FsUnit

type Simple = JsonProvider<""" { "name": "wk", "age": 20 } """>

[<TestFixture>]
type SimpleJsonProviderTest() =

    [<Test>]
    member this.SimpleTest() =
        let simple = Simple.Parse(""" { "name": "kw", "age": 30 } """)
        simple.Age |> should equal 30
        simple.Name |> should equal "kw"