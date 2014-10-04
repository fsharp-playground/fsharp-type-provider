namespace TypeProviderTest

open System
open FSharp.Data
open NUnit.Framework

type Twitter = JsonProvider<"resources/TwitterStream.json", SampleIsList= true>

[<TestFixture>]
type TwitterStreamTest() = 

    [<Test>]
    member this.TestAsyncLoad() =
        let twitter = Twitter.AsyncLoad("https://fsharp.github.io/FSharp.Data/data/TwitterStream.json")
        let x = twitter
        0

    [<Test>]
    member this.TestNumberOfItems() =
        let twitter = Twitter.GetSamples()
        Assert.AreEqual(96, twitter.Length)

    [<Test>]
    member this.TestFirstUser() =
        let twitter = Twitter.GetSamples()
        let user = twitter.[0].User
        Assert.AreEqual(user, "Hailey")


    [<Test>]
    member this.TestFirstTweet() =
        let twitter = Twitter.GetSamples()
        let tweet = twitter.[0].Text
        Assert.AreEqual(tweet,
            Some "I hate this stupid bumper on my iPhone. I want a new case  waahhhhh")

