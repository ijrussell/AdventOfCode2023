open System
open System.IO

type GameData = { Id:int; MaxRed:int; MaxBlue:int; MaxGreen:int }

let (|Split|) (on: string) (input: string) =
    input.Split(on, StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let (|Int|_|) (s: string) =
    match Int32.TryParse s with
    | true, n -> Some(Int n)
    | false, _ -> None

let maxValues = 
    [ "red", 12; "green", 13; "blue", 14 ]
    |> Map.ofList

let parse (input:string) =
    match input with 
    | Split ":" [ Split " " [ _; Int gameId ]; Split ";" sets] -> 
        sets
        |> List.collect (fun set -> set |> (|Split|) ",")
        |> List.fold (fun acc selection ->
            match selection with
            | Split " " [Int qty; colour] -> 
                match colour with
                | "red" when qty > acc.MaxRed -> { acc with MaxRed = qty }
                | "blue" when qty > acc.MaxBlue -> { acc with MaxBlue = qty }
                | "green" when qty > acc.MaxGreen -> { acc with MaxGreen = qty }
                | _ -> acc
            | _ -> failwith $"Unexpected set {selection}"  
        ) { Id = gameId; MaxBlue = 0; MaxRed = 0; MaxGreen = 0 }
    | _ -> failwith $"Not a valid game: {input}"

let validate (data:GameData) =
    let isValid =
        data.MaxBlue <= maxValues["blue"] 
        && data.MaxRed <= maxValues["red"] 
        && data.MaxGreen <= maxValues["green"] 
    if isValid then Some data.Id else None

let calculateProduct (data:GameData) =
    data.MaxBlue * data.MaxGreen * data.MaxRed

let readData (fileName:string) =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines

let runPart1 (fileName:string) =
    fileName
    |> readData
    |> Array.choose (parse >> validate)
    |> Array.sum

let runPart2 (fileName:string) =
    fileName
    |> readData
    |> Array.map (parse >> calculateProduct)
    |> Array.sum

let part1test = runPart1 "test-data-part1.txt"

let part1 = runPart1 "actual-data.txt"

let part2 = runPart2 "actual-data.txt"