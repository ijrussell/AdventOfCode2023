open System
open System.IO

let parse (input:string) =
    match input.Split(":") with
    | [|_;numbers|] -> 
        match numbers.Split("|") with
        | [|winners;chosen|] -> 
            let mine = chosen.Split(" ", StringSplitOptions.RemoveEmptyEntries)
            winners.Split(" ", StringSplitOptions.RemoveEmptyEntries)
            |> Array.filter (fun w -> mine |> Array.contains w)
            |> Array.length
        | _ -> failwith $"Invalid input: {input}"
    | _ -> failwith $"Invalid input: {input}"

let playBonusGames (games:int array) =
    let rec loop acc next =
        match next with 
        | [] -> acc
        | _ -> 
            next
            |> List.collect (fun game->
                let bonus = games[game]
                match bonus with
                | 0 -> []
                | _ -> [game+1..game+bonus]
            )
            |> loop (next @ acc) 
    loop [] (games |> Array.toList |> List.mapi (fun i _ -> i))

let loadData (fileName:string) =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.map parse 

let runPart1 (fileName:string) =
    fileName
    |> loadData 
    |> Array.sumBy (fun len -> pown 2 (len-1))

let runPart2 (fileName:string) =
    fileName
    |> loadData 
    |> playBonusGames
    |> List.length 

let part1test = runPart1 "test-data.txt"

let part1 = runPart1 "actual-data.txt"

let part2test = runPart2 "test-data.txt"

let part2 = runPart2 "actual-data.txt"
