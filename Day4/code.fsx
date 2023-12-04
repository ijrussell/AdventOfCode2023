open System
open System.IO

let calculateWinners (input:string) =
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
    let determineBonusGames (input:int list) =
        input
        |> List.collect (fun game->
            match games[game] with
            | 0 -> []
            | bonus -> [game+1..game+bonus])
    let rec loop acc currentGames =
        match currentGames with 
        | [] -> acc |> List.length
        | _ -> currentGames |> determineBonusGames |> loop (currentGames @ acc) 
    loop [] (List.init (games |> Array.length) id)

let loadData (fileName:string) =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.map calculateWinners 

let runPart1 (fileName:string) =
    fileName
    |> loadData 
    |> Array.sumBy (fun len -> pown 2 (len-1))

let runPart2 (fileName:string) =
    fileName
    |> loadData 
    |> playBonusGames

let part1test = runPart1 "test-data.txt"

let part1 = runPart1 "actual-data.txt"

let part2test = runPart2 "test-data.txt"

let part2 = runPart2 "actual-data.txt"
