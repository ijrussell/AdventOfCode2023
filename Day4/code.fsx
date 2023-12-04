open System
open System.IO

let (|LessThan|Match|GreaterThan|) (first:int list, second:int list) =
    match first, second with
    | hw::_, hc::_ when hc = hw -> Match
    | hw::_, hc::_c when hc < hw -> LessThan
    | _ -> GreaterThan

let findMatches (winners:int list) (chosen:int list) =
    let rec loop acc remW remC =
        match remW, remC with
        | [], _ | _, [] -> acc
        | Match -> loop (remW.Head::acc) remW.Tail remC.Tail
        | LessThan -> loop acc remW remC.Tail
        | GreaterThan -> loop acc remW.Tail remC
    loop [] winners chosen

let parseNumbers (input:string) =
    input.Trim().Split(" ")
    |> Array.choose (fun item -> 
        if String.IsNullOrWhiteSpace item then None 
        else item.Trim() |> int |> Some)
    |> Array.sort
    |> Array.toList

let parse (input:string) =
    match input.Split(":") with
    | [|_;numbers|] -> 
        match numbers.Split("|") with
        | [|winners;chosen|] -> 
            findMatches (winners |> parseNumbers) (chosen |> parseNumbers)
            |> List.length
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
