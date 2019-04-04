module ICPC
open System

let nextElem  = function //Access the next element in the list when an element ends with a ','
    | [] -> "woops"
    | hd :: tl -> hd

let makeList (toConvert : string) = // This takes the string input and splits it into a list of strings (splitting on whitespace)
    toConvert.Split(' ') |> Array.toList


let rec filter predicate = function //building up the list of words that come after a words that ends with a ','
    | [] -> []
    | hd :: tl ->
        match predicate hd with
        | true -> (nextElem tl)::filter predicate tl
        | false -> filter predicate tl


let rec filter2 predicate2 (input : string list) = match input with //building up the list of words that come after a words that ends with a ','
                                                    | [] -> []
                                                    | hd :: tl ->
                                                        match predicate2 hd with
                                                        | true -> (hd.Substring(0, (hd.Length - 1)))::filter2 predicate2 tl
                                                        | false -> filter2 predicate2 tl
    
let lastComma (sting : string) = // The check to see if a string element ends with a ','
    match sting.Substring((sting.Length - 1)) = "," with
            | true -> true
            | _ -> false 

let postComma (sting : string) = // The check to see if a string element ends with a ','
    match sting.Substring((sting.Length - 1)) = "," with
            | true -> true
            | _ -> false 
  
let rec buildpostCommaWords (listless : string list) = match listless with  //building up the list of words that come after a words that ends with a ','
                                                        | [] -> []
                                                        | hd :: tl ->
                                                            match  hd.Substring(hd.Length - 1) with
                                                            | "," -> hd::(hd.Substring(0, (hd.Length - 1)))::(hd.Substring(0, (hd.Length - 1))) + "."::buildpostCommaWords tl
                                                            | "." -> hd::(hd.Substring(0, (hd.Length - 1)))::(hd.Substring(0, (hd.Length - 1))) + ","::buildpostCommaWords tl
                                                            | _ -> hd::hd + "."::hd + ","::buildpostCommaWords tl
                                                           
let findStringContaining2 (text : string) (items : string list) =
    items |> List.tryFind(fun item -> item.Contains(text));;


let rec firstRequirement test (listin : string list) = match listin with 
                                                        | [] -> []
                                                        | hd :: tl ->
                                                            match hd.Substring((hd.Length - 1)) = "," || hd.Substring((hd.Length - 1)) = "." with // see whether the word in question ends with a comma or full stop - if it does, no modification will be necessary as the next word will either be the first word of a sentence or already preceded by a comma
                                                                | true -> hd::firstRequirement test tl // the no modification case
                                                                | _ -> match findStringContaining2 (nextElem tl) test with  // check to see whether next word is in list of words, if not, no modification, if yes, add comma to end of hd 
                                                                            | None -> hd::firstRequirement test tl // no modification needed as next word is not a target word
                                                                            | _ -> hd + ","::firstRequirement test tl // next word is a target word, therefore modify


let rec secondRequirement test (listin : string list) = match listin with 
                                                        | [] -> []
                                                        | hd :: tl -> match hd.Substring((hd.Length - 1)) = "," || hd.Substring((hd.Length - 1)) = "." with // see whether the word in question ends with a comma or full stop - if it does, no modification will be necessary as the next word will either be the first word of a sentence or already preceded by a comma
                                                                        | true -> hd::secondRequirement test tl
                                                                        | _ ->
                                                                             match findStringContaining2 hd test with  // check to see whether next word is in list of words, if not, no modification, if yes, add comma to end of hd 
                                                                                | None -> hd::secondRequirement test tl // no modification needed as next word is not a target word
                                                                                | _ -> hd + ","::secondRequirement test tl // next word is a target word, therefore modify

let rec endsWithFullstop (input : string ) = 
    match input.Substring((input.Length - 1)) = "." with
            | true -> false
            | _ -> true 

let rec startsWithLetter (input : string) =
    match Char.IsLetter(input, 0) with 
        | true -> false
        | _ -> true


//let rec noWhiteSpace (input : string)



let commaSprinkler (input:string) =
    let rec recurseSprinkler inp = // This is the recursive function that keeps applying our rules until we are unable to 
                let listy = makeList inp // Fire up our list of strings from the input string
                let postCommaWords = listy |> filter lastComma // Generate a list of words that are preceded by a comma 
                let morepostCommaWords = postCommaWords |> buildpostCommaWords // Build the list of words preceded by a comma to include all legitimate variants (eg if word = "spot", include "spot," and "spot."
                let preCommaWords = listy |> filter2 postComma  // Generate a list of words that are succeeded by a comma
                let updatedInput = firstRequirement morepostCommaWords listy // test each word in our string list against the list of preceded by comma target words, make changes accordingly
                let part2 = secondRequirement preCommaWords updatedInput // test each word in our string list against the list of succeeded by comma target words, make changes accordingly
                let result = String.concat " " part2 // create a string output of our string list which has had the rules applied to it
                match result = inp with // check to see whether we need to go again 
                            | true -> result
                            | _ -> recurseSprinkler result
    match (input.Length < 2) || (endsWithFullstop input) || (startsWithLetter input) with //Test for all the error cases
        | true -> None 
        | _ -> Some(recurseSprinkler input)
     

























let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
