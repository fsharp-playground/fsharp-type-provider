namespace TypeProviderTest

open System
open FSharp.Data
open NUnit.Framework

type Twitter = JsonProvider<"resources/TwitterStream.json", SampleIsList= true>

[<TestFixture>]
type TwitterStreamTest() = 

    [<Test>]
    member this.TestAsyncLoad() =
        let data = Twitter.AsyncLoad("https://fsharp.github.io/FSharp.Data/data/TwitterStream.json") |> Async.RunSynchronously
        let twitters = data.JsonValue.AsArray()
        let twitter0 = Twitter.Parse(twitters.[0].ToString())
        Assert.AreEqual(Some 0,  twitter0.RetweetCount)

    [<Test>]
    member this.TestNumberOfItems() =
        let twitter = Twitter.GetSamples()
        Assert.AreEqual(96, twitter.Length)

    [<Test>]
    member this.TestFirstUser() =
        let twitter = Twitter.GetSamples()
        let user = twitter.[0].User.Value.Name

        Assert.AreEqual(user, "Hailey")

    [<Test>]
    member this.TestFirstTweet() =
        let twitter = Twitter.GetSamples()
        let tweet = twitter.[0].Text
        Assert.AreEqual(tweet,
            Some "I hate this stupid bumper on my iPhone. I want a new case  waahhhhh")

