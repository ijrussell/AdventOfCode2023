open System.IO

let parse (input:string) =
    match input.Split(": ") with
    | [|_;sets|] -> 
        [|
            for set in sets.Split(";") do
                for selection in set.Split(",") do
                    match selection.Trim().Split(" ") with
                    | [|count;colour|] -> (colour, int count) 
                    | _ -> failwith $"Unexpected set {selection}"  
        |]
        |> Array.groupBy fst
        |> Array.map (fun (_, items) -> items |> Array.maxBy snd |> snd)
        |> Array.reduce (*)
    | _ -> failwith $"Not a valid game: {input}"

let run (fileName:string) =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.map parse
    |> Array.sum

let part2test = run "test-data-part1.txt"

let part2 = run "actual-data.txt"

