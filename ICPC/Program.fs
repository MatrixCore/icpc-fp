module ICPC
open System
open System.Linq

let commaSprinkler (input : string) =
    failwith "Not implemented"

    
     

let rivers (input : string) =
    //(LineWidth, RiverLength)
    let compareRivers (a,b) (c,d) = //Returns the river that longest width
        match b >= d with
        | true -> (a,b)
        | false -> (c,d)
   

    let widthFormat (strInput : string) width = 
    //Function to break up the input string into a list of substrings based on a word width
        let rec format rest final str count =
            match rest with 
            | [] -> final //Return the final string list
            | word::rest -> match count = width with
                            | true -> format rest final@[str.trim()] "" 0 //Add string to the final list and reset str and the counter 
                            | false -> format rest final (str + " " + word) (count + 1)
                            // Concatitate the current word with the carried over string and increment the counter

        wordList = Array.toList(strInput.Split(' ')) //Split entire input into a list of single words
        format wordList [] "" 0


    let findActualWidth strInput wordWidth river = //Returns the characater width of the longest line in a string 
        let a,b = river //Grabs the word width and length of a given river tuple
        let wordList = widthFormat strInput wordWidth
        let rec findWidth rest maxLen =
            match rest with
            | [] -> maxLen
            | elem::rest -> match elem.Length > maxLen with
                            | false -> findWidth rest maxLen
                            | true -> findWidth rest elem.Length

        let num = findWidth wordList 0
        num,b //Returns a tuple of actual width and river length


    let findAllSpaces (str : string) = //Finds all ' ' characters that appear in a single string and returns a list of indexes
        let rec findSpaces str listSpace =
            match str.IndexOf(' ') with
            | -1 -> listSpace //If there are no more spaces in this string, return the list of indexes of spaces in the given string 
            | num -> findSpaces (str.Substring num) listSpace@num //If there are still spaces to find, recurse
        
        findSpaces str []


    let findRiverLength = failwith "Not done"
            

    let checkDoubleSpace spaceList = //Returns true if there are two ' ' characters next to each other
        let checkSpace rest previous =
            match rest with
            | [] -> false
            | elem::rest -> match (elem - previous) with //Subtract the index of the current space from the previous
                            | 1 -> true //If the difference is only one, then it's a double space 
                            | _ -> checkSpace rest elem //Otherwise keep looking

        let first::rest = spaceList 
        checkSpace rest first

    let validString input = 
        match a = input.Split(' ').Length < 2 with //Checks if there is more than a single word in the supplied 
        | true -> false
        | false -> match a.Max > 80 with //Checks if a single world is greater than 80 characters
                   | true -> false
                   | false -> match (findAllSpaces input) |> checkDoubleSpace with //Looks for double spaces in the supplied string
                              | true -> false
                              | false -> true
        

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
