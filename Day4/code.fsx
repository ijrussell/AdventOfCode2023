open System
open System.IO

let (|Split|) (on: string) (input: string) =
    input.Split(on, StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let calculateWinners (input:string) =
    match input with
    | Split ":" [ _; Split "|" [ Split " " winners; Split " " chosen ] ] ->
        winners
        |> List.filter (fun w -> chosen |> List.contains w)
        |> List.length
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
