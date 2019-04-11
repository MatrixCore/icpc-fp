module ICPC.Tests

open NUnit.Framework
open FsUnit
(*
[<Test>]
let ``Comma sprinkler, input 1`` () =
    commaSprinkler "please sit spot. sit spot, sit. spot here now here."
    |> should equal (Some "please, sit spot. sit spot, sit. spot, here now, here.")

[<Test>]
let ``Comma sprinkler, input 2`` () =
    commaSprinkler "one, two. one tree. four tree. four four. five four. six five."
    |> should equal (Some "one, two. one, tree. four, tree. four, four. five, four. six five.")

[<Test>]
let ``Comma sprinkler, input 3`` () =
    commaSprinkler "test." |> should equal (Some "test.")

[<Test>]
let ``Comma sprinkler, error case 1`` () =
    commaSprinkler "" |> should equal None
    commaSprinkler "o" |> should equal None

[<Test>]
let ``Comma sprinkler, error case 2`` () =
    commaSprinkler "tesT." |> should equal None
    commaSprinkler "test" |> should equal None
    commaSprinkler "to be, or not to be? that is the question." |> should equal None

[<Test>]
let ``Comma sprinkler, error case 3`` () =
    commaSprinkler " nope." |> should equal None
    commaSprinkler ", as they say." |> should equal None

[<Test>]
let ``Comma sprinkler, error case 4`` () =
    commaSprinkler "one, two.  one tree." |> should equal None
    commaSprinkler "one,two. one tree." |> should equal None
    commaSprinkler "one ,two. one tree." |> should equal None
    commaSprinkler "one, two... one tree." |> should equal None
    commaSprinkler "one, two. . one tree." |> should equal None
    commaSprinkler "one, two. one tree ." |> should equal None

[<Test>]
let ``Comma sprinkler, error case 5`` () =
        commaSprinkler "one, two. one tree. " |> should equal None
        commaSprinkler "one, two. one tree, " |> should equal None
        commaSprinkler "one, two. one tree " |> should equal None 
        
*)

[<Test>]
let ``Rivers, input 1`` () =
    rivers "The Yangtze is the third longest river in Asia and the longest in the world to flow entirely in one country"
    |> should equal (Some (15, 5))

[<Test>]
let ``Rivers, input 2`` () =
    rivers "When two or more rivers meet at a confluence other than the sea the resulting merged river takes the name of one of those rivers"
    |> should equal (Some (21, 6))

[<Test>]
let ``Rivers, input 3`` () =
    rivers "hello world" |> should equal (Some (11, 1))

[<Test>]
let ``Rivers, error case 1`` () =
    rivers "" |> should equal None
    rivers "testing" |> should equal None

[<Test>]
let ``Rivers, error case 2`` () =
    rivers "hello, world" |> should equal None
    rivers "hello world!" |> should equal None

[<Test>]
let ``Rivers, error case 3`` () =
    rivers "hi  world" |> should equal None

[<Test>]
let ``Rivers, error case 4`` () =
    rivers " hello world" |> should equal None
    rivers "hello world " |> should equal None

[<Test>]
let ``Rivers, error case 5`` () =
    rivers "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabc test" |> should equal None