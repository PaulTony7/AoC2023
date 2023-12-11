open System.IO
open System
open System.Linq

let lines = 
    File.ReadAllLines("Input.txt") 
    |> Array.toList 
    |> List.map (fun x -> x.Split([|' '|]) |>  Array.toList |> List.map (fun x -> x |> int64 |> bigint ))

let isAllZero (l:bigint list) = 
    l |> List.forall (fun x -> x=0I)
let rec calculateDifferenceList (l:bigint list) = 
    match l with
    | x::y::_ -> y - x::calculateDifferenceList l.Tail
    | _ -> []

let rec getNextInSequence (l:bigint list) =
    if isAllZero l then 0I
    else 
        l.Head - ((calculateDifferenceList l) |> getNextInSequence)

lines |> List.map (fun x -> getNextInSequence x) |> List.sum |> printfn "sum %A" 