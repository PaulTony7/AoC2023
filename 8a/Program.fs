open System
open System.IO
open System.Collections.Generic
open System.Linq

type Fork =
    val left:string
    val right:string
    new (x,y) =
        {
            left = x;
            right = y
        }

let filePath = "Input.txt"
let inputFile = File.ReadLines(filePath)
let instructions = inputFile.First()
let nodes = inputFile.Skip(2) |> Seq.toList
let dict = new Dictionary<string, Fork>()
nodes 
|> List.map (fun x -> x.Split([|'=';'(';')';',';' '|], StringSplitOptions.RemoveEmptyEntries))
|> List.iter (fun x -> dict.Add(x[0], new Fork(x[1], x[2])))
let rec traverseTree (l:char list) (w:string) (counter:int):int= 
    match l[counter%l.Length] with
    | 'L' ->
        if dict[w].left = "ZZZ" then (counter+1)
        else 
            traverseTree l (dict[w].left) (counter+1)
    | 'R' ->
        if dict[w].right = "ZZZ" then (counter+1)
        else 
            traverseTree l (dict[w].right) (counter+1)
    | _ -> -1

traverseTree (instructions.ToCharArray() |>Array.toList) "AAA" 0 |> printf "Total %i\n" 