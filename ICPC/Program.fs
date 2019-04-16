module ICPC
open System
open System.Linq
    
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


let rec firstRequirement test (listin : string list) = match listin with //modifying our string list to comply with rule 1
                                                        | [] -> []
                                                        | hd :: tl ->
                                                            match hd.Substring((hd.Length - 1)) = "," || hd.Substring((hd.Length - 1)) = "." with // see whether the word in question ends with a comma or full stop - if it does, no modification will be necessary as the next word will either be the first word of a sentence or already preceded by a comma
                                                                | true -> hd::firstRequirement test tl // the no modification case
                                                                | _ -> match findStringContaining2 (nextElem tl) test with  // check to see whether next word is in list of words, if not, no modification, if yes, add comma to end of hd 
                                                                            | None -> hd::firstRequirement test tl // no modification needed as next word is not a target word
                                                                            | _ -> hd + ","::firstRequirement test tl // next word is a target word, therefore modify


let rec secondRequirement test (listin : string list) = match listin with //modifying our string list to comply with rule 2
                                                        | [] -> []
                                                        | hd :: tl -> match hd.Substring((hd.Length - 1)) = "," || hd.Substring((hd.Length - 1)) = "." with // see whether the word in question ends with a comma or full stop - if it does, no modification will be necessary as the next word will either be the first word of a sentence or already preceded by a comma
                                                                        | true -> hd::secondRequirement test tl
                                                                        | _ ->
                                                                             match findStringContaining2 hd test with  // check to see whether next word is in list of words, if not, no modification, if yes, add comma to end of hd 
                                                                                | None -> hd::secondRequirement test tl // no modification needed as next word is not a target word
                                                                                | _ -> hd + ","::secondRequirement test tl // next word is a target word, therefore modify

let rec endsWithFullstop (input : string ) =  // test to see if input ends with a fullstop 
    match input.Substring((input.Length - 1)) = "." with
            | true -> false
            | _ -> true 

let rec startsWithLetter (input : string) = // the test to see if the input starts with a letter
    match Char.IsLetter(input, 0) with 
        | true -> false
        | _ -> true

let nextCharElem  = function //Access the next element in the list 
    | [] -> ' '
    | hd :: tl -> hd

let noWhiteSpace (input : string) = 
    let charArr = input.ToCharArray();  
    let charList = charArr |> Array.toList
    let rec checkSpace  = function
            | [] -> false
            | hd :: tl ->
                match (hd = '?') || ((hd = ' ') && ((nextCharElem tl) = ' ')) || ((hd = '.') && ((nextCharElem tl) = '.')) || Char.IsUpper(hd) || ((hd = ',') && ((nextCharElem tl) <> ' ')) || ((hd = ' ') && ((nextCharElem tl) = '.')) with 
                    | true -> true 
                    | _ -> checkSpace tl

    checkSpace charList


let commaSprinkler (input:string) =
    let rec recurseSprinkler inp = // This is the recursive function that keeps applying our rules until we are unable to 
                let listy = makeList inp // Fire up our list of strings from the input string
                let postCommaWords = listy |> filter lastComma // Generate a list of words that are preceded by a comma 
                let morepostCommaWords = postCommaWords |> buildpostCommaWords // Build the list of words preceded by a comma to include all legitimate variants (eg if word = "spot", include "spot," and "spot."
                let preCommaWords = listy |> filter2 postComma  // Generate a list of words that are succeeded by a comma
                let updatedInput = firstRequirement morepostCommaWords listy // test each word in our string list against the list of preceded by comma target words, make changes accordingly
                let part2 = secondRequirement preCommaWords updatedInput // test each word in our string list against the list of succeeded by comma target words, make changes accordingly
                let result = String.concat " " part2 // create a string output of our string list which has had the rules applied to it
                match result = inp with // check to see whether we need to go again (if the string result is the same as inp, then we know that our attempts to insert new commas did not succeed, therefore nothing left to do
                            | true -> result // return the current version of the string if no more comma modifications are possible
                            | _ -> recurseSprinkler result // there could still be further comma insertions, so go again
    match (input.Length < 2) || (endsWithFullstop input) || (startsWithLetter input) || (noWhiteSpace input) with //Test for all the error cases
        | true -> None 
        | _ -> Some(recurseSprinkler input)

let rivers (input : string) =
    //(LineWidth, RiverLength)
    let compareRivers (a,b) (c,d) = //Returns the river that longest width
        match b >= d with
        | true -> (a,b)
        | false -> (c,d)


    let findActualWidth strInput wordWidth river = () //Returns the character width of the longest line in a string 
    //Redo


    //Fix this
    let findAllSpaces (input : string) = //Finds all ' ' characters that appear in a single string and returns a list of indexes
        let charList = Array.toList(input.ToCharArray())
        let rec space count (charL : char list) (spaceList : int list) =
            match count = (charL.Length - 1) with
            | true -> spaceList
            | false -> match charL.Item(count) = ' ' with
                       | true -> space (count + 1) charL (spaceList@[count])
                       | false -> space (count + 1) charL spaceList

        space 0 charList []

    //Simply this function using the already created findAllSpaces int list
    let checkDoubleSpace spaceList = //Returns true if there are two ' ' characters next to each other
        let rec checkSpace rest previous =
            match rest with
            | [] -> false
            | elem::rest -> match (elem - previous) with //Subtract the index of the current space from the previous
                            | 1 -> true //If the difference is only one, then it's a double space 
                            | _ -> checkSpace rest elem //Otherwise keep looking

        let first::rest = spaceList 
        checkSpace rest first
    
    let checkChar (input : string) =
        let rec char rest =
            match rest with
            | [] -> true
            | elem::rest -> match Char.IsWhiteSpace(elem) || Char.IsLetter(elem) with
                            | true -> char rest
                            | false -> false
        
        char (List.ofArray(input.ToCharArray()))

    let validString (input : string) = 
        match input.Split(' ').Length < 2 with //Checks if there is more than a single word in the supplied 
        | true -> false
        | false -> match checkChar input with //Looks for
                   | false -> false
                   | true -> match Array.exists(fun (elem : string) -> elem.Length > 80) (input.Split(' ')) with //Checks if a single world is greater than 80 characters
                             | true -> false
                             | false -> match input.Last() = ' ' || input.First() = ' ' with //Checks for trailing or leading spaces
                                        | true -> false
                                        | false -> match (findAllSpaces input) |> checkDoubleSpace with //Looks for double spaces in the supplied string
                                                   | true -> false
                                                   | false -> true


    let calcRiver (spaceList : int list) = ()
        

    let findRiver (spaceList : int list) width =
        let count = 1
        let pos = 0

        let diff = ((spaceList.Item(pos + width) - spaceList.Item((width - 1) * count)) - spaceList.Item(pos))
        //weird line calculation to find the relative space postions 
        match diff > 2 && diff < -2 with
        | false -> ()// Not a river
        | true -> () // Forms a valid river, find a way to store river values
                     // Also check if the last index is in the list already
        //Maybe a list of river tuples (riverLength, lastSpaceIndex)?

    
    
    match validString input with
    | false -> None
    | _ -> //Call the other functions
           Some(input |> findAllSpaces |> findRiver)
    
    

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    Console.ReadLine ()
    0 // return an integer exit code
