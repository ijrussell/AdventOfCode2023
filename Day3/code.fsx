open System
open System.IO

let isNumeric (input:string) =
    let (success, _) = Int32.TryParse(input)
    success

let hasAdjacentSymbol (symbols:(int * int * string) array) (item:(int * int * string)) =
    let (i, j, _) = item
    [
        for di in -1..1 do
            for dj in -1..1 do
                symbols
                |> Array.exists (fun (x, y, _) -> x = i + di && y = j + dj)
    ]
    |> List.filter (fun value -> value)
    |> fun items -> items <> []

let joinNumbers (numbers:(int * int * string * bool) list) =
    let rec loop acc rem current =
        match rem, current with
        | [], _ -> current::acc
        | head::tail, [] ->  loop acc tail [head]
        | head::tail, hc::_ -> 
            let (hi, hj, hv, _) = head
            let (ci, cj, _, _) = hc
            if hi = ci && hj = cj + 1 then loop acc tail (head::current)
            else loop (current::acc) tail [head]
    loop [] numbers []

let combine (parts:(int * int * string * bool) list list) =
    parts
    |> List.map List.rev 
    |> List.rev
    |> List.map (fun items ->
        items
        |> List.fold (fun (str, inc) (_, _, nstr, ninc) -> str + nstr, inc || ninc) ("", false)
        |> fun (str, inc) -> int str, inc
    )

let run (fileName:string) =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.mapi (fun i data -> 
        data 
        |> Seq.mapi (fun j item -> (i, j, string item))
        |> Seq.toArray
    )
    |> Array.concat
    |> Array.filter (fun (_, _, item) -> item <> ".")
    |> Array.partition (fun (_, _, item) -> item |> isNumeric)
    |> fun (numbers, symbols) ->
        numbers
        |> Array.map (fun (i, j, number) -> (i, j, number, hasAdjacentSymbol symbols (i, j, number)))
    |> Array.toList
    |> joinNumbers
    |> combine
    |> List.choose (fun (value, inc) -> if inc then Some value else None)
    |> List.sum

let part1test = run "test-data-part1.txt"

let part1 = run "actual-data.txt"
