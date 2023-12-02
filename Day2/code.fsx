open System.IO

let cubes = 
    [ "red", 12; "green", 13; "blue", 14 ]
    |> Map.ofList

let parse (mapping:Map<string,int>) (input:string) =
    match input.Split(": ") with
    | [|game;sets|] -> 
        [|
            for set in sets.Split(";") do
                for selection in set.Split(",") do
                    match selection.Trim().Split(" ") with
                    | [|num;col|] -> (game, num, col) 
                    | _ -> failwith $"Unexpected set {selection}"  
        |]
        |> Array.forall (fun (_, num, col) -> int num <= mapping[col])
        |> fun isValid -> if isValid then Some (game.Substring(5) |> int) else None
    | _ -> failwith $"Not a valid game: {input}"

let run (mapping:Map<string,int>) (fileName:string) =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.choose (parse mapping)
    |> Array.sum

let part1test = run cubes "test-data-part1.txt"

let part1 = run cubes "actual-data.txt"