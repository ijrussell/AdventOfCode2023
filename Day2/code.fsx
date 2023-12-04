open System
open System.IO

let (|Split|) (on: string) (input: string) =
    input.Split(on, StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let (|Int|_|) (s: string) =
    match Int32.TryParse s with
    | true, n -> Some(Int n)
    | false, _ -> None

let cubes = 
    [ "red", 12; "green", 13; "blue", 14 ]
    |> Map.ofList

let inline readSelections (input:string list) =
    input
    |> List.collect (fun selection ->
        match selection with
        | Split "," parts -> 
            parts
            |> List.map (fun part ->
                match part with
                | Split " " [Int value; colour] -> (value, colour)
                | _ -> failwith $"Unexpected part {part}"
            )  
    ) 

let parse (mapping:Map<string,int>) (input:string) =
    match input with
    | Split ":" [ Split " " [_; Int gameId]; Split ";" selections] -> 
        selections
        |> readSelections
        |> List.exists (fun (value, colour) -> value > mapping[colour])
        |> fun isInvalid -> if isInvalid then None else Some gameId
    | _ -> failwith $"Not a valid game: {input}"

let run (mapping:Map<string,int>) (fileName:string) =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.choose (parse mapping)
    |> Array.sum

let part1test = run cubes "test-data-part1.txt"

let part1 = run cubes "actual-data.txt"
