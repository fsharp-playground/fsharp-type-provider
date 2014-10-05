namespace TypeProviderTest

open NUnit.Framework
open FsUnit

[<TestFixture>]
type TestFsUnit() = 

    [<Test>]
    member this.Test() =
        1 |> should equal 1