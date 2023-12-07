open System.IO
open System

let rec firstNumberInLine (l: char list) : int = 
    match l with
    | [] -> 0
    | x::xs -> if Char.IsNumber(x) then (int x - int '0') else firstNumberInLine xs
let twoDigitNumberInLine l = (firstNumberInLine l) * 10 + firstNumberInLine (l |> List.rev)
let sumAllLinesInFile x =
    (File.ReadAllLines(x) |> Array.toList) 
    |> List.map (fun x -> twoDigitNumberInLine (Seq.toList(x))) 
    |> List.sum

printf "%i" (sumAllLinesInFile "Input.txt")