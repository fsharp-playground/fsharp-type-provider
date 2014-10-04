namespace TypeProviderTest

open NUnit.Framework
open FsUnit.TopLevelOperators

[<TestFixture>]
type TestFsUnit = 

    [<Test>]
    member this.Test() =
        "x" |> should equal "Y"