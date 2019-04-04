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

    
let lastComma (sting : string) = // The check to see if a string element ends with a ','
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
                                                           


let commaSprinkler input =

    let listy = makeList input // Fire up our list
    let postCommaWords = listy |> filter lastComma // Generate a list of words that are preceded by a comma 
    let morepostCommaWords = postCommaWords |> buildpostCommaWords // Build the list to include all legitimate variants (eg if word = "spot", include "spot," and "spot."
    morepostCommaWords
    
    
    
    
    //postCommaWords |> List.map (fun x -> match (x.Length - 1) != ',' || '.' with  //need to add instances of each word that could be valid - ending without , or . and ending with . and ,
                                                //| true -> 
                                                //| _ -> 
  








let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
