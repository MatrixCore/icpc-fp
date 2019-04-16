module ICPC
open System
open System.Linq
    
let commaSprinkler input =
    failwith "Not implemented"



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
    0 // return an integer exit code
