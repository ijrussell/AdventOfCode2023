open System.IO

let parse (mapping:Map<string,int>) (input:string) =
    match input.Split(": ") with
    | [|_;sets|] -> 
        [|
            for set in sets.Split(";") do
                for selection in set.Split(",") do
                    match selection.Trim().Split(" ") with
                    | [|num;col|] -> (num, col) 
                    | _ -> failwith $"Unexpected set {selection}"  
        |]
        |> Array.groupBy snd
        |> Array.map (fun (_, items) ->
            items |> Array.maxBy (fun (num, _) -> int num) |> fst |> int 
        )
        |> Array.reduce (*)
    | _ -> failwith $"Not a valid game: {input}"

let run (mapping:Map<string,int>) (fileName:string) =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.map (parse mapping)
    |> Array.sum

let part2test = run cubes "test-data-part1.txt"

let part2 = run cubes "actual-data.txt"

