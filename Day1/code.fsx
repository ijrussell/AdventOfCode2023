open System
open System.IO
open System.Collections.Generic

module Part1 =
    let mapping = 
        [1..9]
        |> List.map (fun item -> string item, item)
        |> dict

module Part2 =
    let mapping = 
        [ "one", 1; "two", 2; "three", 3; "four", 4; "five", 5; "six", 6; "seven", 7; "eight", 8; "nine", 9 ]
        |> dict

let inline buildSearchValues (mapping:IDictionary<string,int>) = 
    let values = mapping.Values |> Seq.map string
    let keys : string seq = mapping.Keys
    Seq.concat [ keys; values ]
    |> Seq.distinct

let inline parseFirst (seachValues:string seq) (input:string) =
    seachValues 
    |> Seq.map (fun item -> item, input.IndexOf item)
    |> Seq.filter (fun item -> snd item >= 0)
    |> Seq.minBy snd 
    |> fst

let inline parseLast (seachValues:string seq) (input:string) =
    seachValues
    |> Seq.map (fun item -> item, input.LastIndexOf item)
    |> Seq.maxBy snd
    |> fst 

let parse (mapping:IDictionary<string,int>) =
    let searchValues = buildSearchValues mapping
    fun (input:string) ->
        let getValue (input:string) =
            let (success, value) = Int32.TryParse(input)
            if success then value 
            else mapping[input]
        let calc f = input |> f searchValues |> getValue
        10 * calc parseFirst + calc parseLast

let run (mapping:IDictionary<string,int>) (fileName:string) =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.map (parse mapping)
    |> Array.sum

let part1 = run Part1.mapping "actual-data.txt" // 55386

let part2 = run Part2.mapping "actual-data.txt" // 54824
