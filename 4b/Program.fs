open System.IO
open System
open System.Collections.Generic

let isNumberWinning (x:int) (winning:int list) =
    if winning |> List.contains x then 1 else 0

let filePath = "Input.txt"
let splitInput =
     File.ReadAllLines(filePath) 
    |> Array.toList 
    |> List.map (fun x -> x.Split([|':';'|'|]))
let accumulatedCards = new List<int>(splitInput.Length)
for i in 1..splitInput.Length do
    accumulatedCards.Add(1)
printf "length %i\n" accumulatedCards[1]

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

(runningNumbers, winningNumbers) ||> List.mapi2 (fun i x z ->
(0, x) ||> List.fold (fun acc y -> acc + isNumberWinning y z))
|> List.iteri (fun i e -> 
    for j in 1..e do
        accumulatedCards[i+j] <- accumulatedCards[i] + accumulatedCards[i+j] 
)
accumulatedCards.ToArray() |> Array.toList |> List.sum |> printf "sum cards %i\n"