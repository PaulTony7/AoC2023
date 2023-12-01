open System.IO
open System

let stringlist (l:char list) =
    System.String.Concat(Array.ofList(l))

let rec spelled (l: string) =
    match (l[..2]) with
    | "" -> -1
    | "one" -> 1
    | "six" -> 6
    | "two" -> 2
    |  _ -> 
        match (l[..3]) with
            | "four" -> 4
            | "five" -> 5
            | "nine" -> 9
            | "zero" -> 0
            | _ ->
            match(l[..4]) with
                | "seven" -> 7
                | "eight" -> 8
                | "three" -> 3
                | _ -> (spelled l[1..])

let rec firstNumberInLine (l: char list) (aggregated :char list) (map: list<char> -> list<char>) : int = 
    match l with
    | [] -> 0
    | x::xs -> 
        if Char.IsNumber(x) 
        then 
            (int x - int '0') 
        else 
            if spelled <| stringlist(aggregated@[x] |> map) = -1 then
                firstNumberInLine xs (aggregated@[x]) map
            else 
                spelled <| stringlist(aggregated@[x] |> map)

let twoDigitNumberInLine l = 
    (firstNumberInLine l [] (fun x -> x) ) * 10  
    + firstNumberInLine (l |> List.rev) [] List.rev 

let sumAllLinesInFile x =
    (File.ReadAllLines(x) |> Array.toList) 
    |> List.map (fun x -> twoDigitNumberInLine (Seq.toList(x))) 
    |> List.sum

printf "%i" (sumAllLinesInFile "Input.txt")