open System
open System.IO

let isNumeric (input:string) =
    let (success, _) = Int32.TryParse(input)
    success

let hasAdjacentSymbol (symbols:(int * int * string) list) (item:(int * int * string)) =
    let (i, j, _) = item
    [
        for di in -1..1 do
            for dj in -1..1 do
                symbols
                |> List.exists (fun (x, y, _) -> x = i + di && y = j + dj)
    ]
    |> List.filter (fun value -> value)
    |> fun items -> items <> []

let hasAdjacentGear (gear:int * int) (numbers:(int * int) list) =
    let (x, y) = gear
    [
        for i, j in numbers do
            for dx in -1..1 do
                for dy in -1..1 do
                    if x + dx = i && y + dy = j then (i, j)
    ]
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
        |> List.fold (fun (str, inc, pos) (i, j, nstr, ninc) -> str + nstr, inc || ninc, (i, j)::pos) ("", false, [])
        |> fun (str, inc, pos) -> int str, inc, pos
    )

let readData (fileName:string) =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> fun rows ->
        [
            for i in 0..rows.Length-1 do
                for j in 0..rows[i].Length-1 do
                    let item = rows[i][j]
                    if item <> '.' then (i, j, string item)
        ]
    |> List.partition (fun (_, _, item) -> item |> isNumeric)

let runPart1 (fileName:string) =
    fileName
    |> readData
    |> fun (numbers, symbols) ->
        numbers
        |> List.map (fun (i, j, number) -> (i, j, number, hasAdjacentSymbol symbols (i, j, number)))
    |> joinNumbers
    |> combine
    |> List.choose (fun (value, inc, _) -> if inc then Some value else None)
    |> List.sum

let runPart2 (fileName:string) =
    let (numbers, symbols) =
        fileName
        |> readData
    let gears = symbols |> List.filter (fun (_, _, item) -> item = "*")
    let adjacent =
        numbers
        |> List.map (fun (i, j, number) -> (i, j, number, hasAdjacentSymbol gears (i, j, number)))
        |> joinNumbers
        |> combine
        |> List.filter (fun (_, isAdjacent, _) -> isAdjacent)
    gears 
    |> List.map (fun (i, j, _) -> 
        adjacent 
        |> List.filter (fun (_, _, items) ->
            hasAdjacentGear (i, j) items
        )
    )
    |> List.filter (fun items -> 
        items 
        |> List.filter (fun (_, _, nums) -> nums |> List.length > 1)
        |> List.length > 1
    )
    |> List.map (fun items -> 
        items 
        |> List.map (fun (num, _, _) -> num)
        //|> List.reduce (*)
    )
    //|> List.sum

let part1test = runPart1 "test-data-part1.txt"

let part1 = runPart1 "actual-data.txt"

let part2test = runPart2 "test-data-part1.txt"

let part2 = runPart2 "actual-data.txt"
