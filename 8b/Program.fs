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
let rec gcdBig (a: bigint) (b: bigint) =
    if b = bigint.Zero then a
    else gcdBig b (a % b)

let rec lcmBig (a: bigint) (b: bigint) =
    if a = bigint.Zero || b = bigint.Zero then bigint.Zero
    else (a * b) / (gcdBig a b)

let rec listLcmBig (numbers: bigint list) =
    match numbers with
    | [] -> bigint.One
    | hd :: tl -> lcmBig hd (listLcmBig tl)

let filePath = "Input.txt"
let inputFile = File.ReadLines(filePath)
let instructions = inputFile.First()
let nodes = 
    inputFile.Skip(2) 
    |> Seq.toList
    |> List.map (fun x -> x.Split([|'=';'(';')';',';' '|], StringSplitOptions.RemoveEmptyEntries))

let dict = new Dictionary<string, Fork>()
nodes 
|> List.iter (fun x -> dict.Add(x[0], new Fork(x[1], x[2])))
let beginingNodes =
    dict.Keys |> Seq.filter (fun x-> x[2] = 'A') |> Seq.toList
let isCorrectNode (w:string) =
    w[2] = 'Z'
let rec traverseTree (l:char list) (w:string) (counter:bigint):bigint= 
    match l[(counter|>int)%l.Length] with
    | 'L' ->
        if isCorrectNode dict[w].left then (counter+1I)
        else 
            traverseTree l (dict[w].left) (counter+1I)
    | 'R' ->
        if isCorrectNode dict[w].right then (counter+1I)
        else 
            traverseTree l (dict[w].right) (counter+1I)
    | _ -> -1I

List.map (fun x -> traverseTree (instructions.ToCharArray() |>Array.toList) x 0I) beginingNodes 
|> listLcmBig 
|> printf "Total %A\n" 