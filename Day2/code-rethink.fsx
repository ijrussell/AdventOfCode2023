open System.IO

type GameData = { Id:int; MaxRed:int; MaxBlue:int; MaxGreen:int }

let maxValues = 
    [ "red", 12; "green", 13; "blue", 14 ]
    |> Map.ofList

let parse (input:string) =
    match input.Split(": ") with
    | [|game;sets|] -> 
        let id = game.Substring("Game ".Length) |> int
        sets.Split(";")
        |> Array.collect (fun set -> set.Split(","))
        |> Array.fold (fun acc selection ->
            match selection.Trim().Split(" ") with
            | [|quantity;colour|] -> 
                let qty = int quantity
                match colour with
                | "red" when qty > acc.MaxRed -> { acc with MaxRed = qty }
                | "blue" when qty > acc.MaxBlue -> { acc with MaxBlue = qty }
                | "green" when qty > acc.MaxGreen -> { acc with MaxGreen = qty }
                | _ -> acc
            | _ -> failwith $"Unexpected set {selection}"  
        ) { Id = id; MaxBlue = 0; MaxRed = 0; MaxGreen = 0 }
    | _ -> failwith $"Not a valid game: {input}"

let validate (data:GameData) =
    let isValid =
        data.MaxBlue <= maxValues["blue"] 
        && data.MaxRed <= maxValues["red"] 
        && data.MaxGreen <= maxValues["green"] 
    if isValid then Some data.Id else None

let calculateProduct (data:GameData) =
    data.MaxBlue * data.MaxGreen * data.MaxRed

let runPart1 (fileName:string) =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.choose (parse >> validate)
    |> Array.sum

let runPart2 (fileName:string) =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.map (parse >> calculateProduct)
    |> Array.sum

let part1test = runPart1 "test-data-part1.txt"

let part1 = runPart1 "actual-data.txt"

let part2 = runPart2 "actual-data.txt"