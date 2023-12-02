open System
open System.IO

module Part1 =
    let mapping = 
        [1..9]
        |> List.map (fun item -> string item, item)
        |> Map.ofList

module Part2 =
    let mapping = 
        ([1..9] |> List.map (fun item -> string item, item)) @
        [ "one", 1; "two", 2; "three", 3; "four", 4; "five", 5; "six", 6; "seven", 7; "eight", 8; "nine", 9 ]
        |> Map.ofList

let parse (mapping:Map<string,int>) (input:string) =
    let getValue (key:string) =
        let (success, value) = Int32.TryParse(key)
        if success then value else mapping[key]
    let matches = [
        for i in 0..input.Length-1 do
            for item in mapping.Keys do
                if input.Substring(i).StartsWith(item) then (item, i) ]
    let first = matches |> List.minBy snd |> fst
    let last = matches |> List.maxBy snd |> fst
    (10 * (first |> getValue)) + (last |> getValue)

let run (mapping:Map<string,int>) (fileName:string) =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.map (parse mapping)
    |> Array.sum

let part1 = run Part1.mapping "actual-data.txt" // 55386

let part2 = run Part2.mapping "actual-data.txt" // 54824
