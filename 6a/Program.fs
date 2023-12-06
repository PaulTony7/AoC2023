open System.IO
open System

let lines = File.ReadAllLines("Input.txt") |> Array.toList
let record = (lines[1].Split([|':'|])[1]).Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList |> List.map (fun x -> x |> int64)
let times = (lines[0].Split([|':'|])[1]).Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList |> List.map (fun x -> x |> int64)

let getRange (l:double list) = 
    if (l[1] > double(l[1] |> int64)) then
        (l[1] |> int64) - (l[0] |> int64)
    else 
        (l[1] |> int64) - (l[0] |> int64) - int64(1)
let delta timeTotal (record:int64) =
    timeTotal * timeTotal - (int64(4) * record)

let getCrossings timeTotal record =
    let d = delta timeTotal record
    if d > 0 then 
        ((double(timeTotal) - Math.Sqrt(d|>double)) / 2.)::((double(timeTotal) + Math.Sqrt(d|>double)) / 2.)::[]
    else if d = 0 then
        (double(timeTotal)/2.)::[]
    else
        []
List.map2 (fun x y -> getRange (getCrossings x y) ) times record 
    |> List.fold (fun acc x -> x*acc) (1 |>int64) |> printf "Range: %A\n" 