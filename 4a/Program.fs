open System.IO
open System

let isNumberWinning (x:int) (winning:int list) =
    if winning |> List.contains x then 1 else 0

let filePath = "Input.txt"
let splitInput =
     File.ReadAllLines(filePath) 
    |> Array.toList 
    |> List.map (fun x -> x.Split([|':';'|'|]))
let collectNumbers (i:int) = 
    splitInput
    |> List.map (fun x -> x[i])
    |> List.map (
        fun x -> 
            (x.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)) 
            |> Array.toList 
            |> List.map (fun y -> y |> int))
let runningNumbers = collectNumbers 1
let winningNumbers = collectNumbers 2

(runningNumbers, winningNumbers) ||> List.map2 (fun x z ->
(0, x) ||> List.fold (fun acc y -> acc + isNumberWinning y z))
|> List.map (fun x -> if x = 0 then 0 else 1 <<< (x-1))
|> List.sum |> printf "%i\n"