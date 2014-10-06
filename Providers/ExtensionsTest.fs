namespace TypeProviderTest
open NUnit.Framework

module X =
     let right n (x:string) =
        if x.Length <= 2 then x
        else x.Substring(x.Length - n)


[<TestFixture>]
type ExtensionsTest() =
    [<Test>]
    member this.TestRight5() =
        let x = "0123456789" |> X.right 5
        Assert.AreEqual("56789", x)

    [<Test>]
    member this.TestRight() =
        let x = X.right 2
        Assert.AreEqual("10", x "010")


