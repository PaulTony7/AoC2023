open System
open System.Linq
open System.IO

let lines = File.ReadAllLines("Input.txt") |> Array.toList |> List.map (fun x -> x.Split([|' '|]))
let springs = lines |> List.map (fun x -> x[0].ToCharArray() |> Array.toList) 
let groups = lines |> List.map (fun x -> x[1].Split([|','|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList |> List.map (fun x -> int x))

let rec checkIfPossible (l:char list) (g:int list) (acc:int) : bool=
    match l with
    | x::xs ->
        if x = '.' then
            if acc > 0 && g.Head > acc then false
            else
                if acc > 0 && g.Head = (acc) then
                    checkIfPossible xs g.Tail 0
                else
                    checkIfPossible xs g 0
        else
            if g = [] || g.Head < (acc+1) then false
            else checkIfPossible xs g (acc+1)
    | _ -> if g = [] || g.Length = 1 && g.Head = acc then true else false

let getBit (i:int) (index:int) =
    i &&& (1 <<< index) > 0

let rec createInstance (l:char list) (i:int) (index:int) =
    match l with
    | x::xs -> 
        if x = '?' then (if (getBit i index) then '#' else '.')::createInstance xs i (index+1)
        else x::createInstance xs i index
    | _ -> []
let checkAllCombinations (g:int list) (l:char list)  =
    let qCount = l.Where((fun x -> x = '?')).Count()
    let combinations = 2 <<< (qCount-1)
    let mutable acc = 0
    for i in 0..(combinations-1) do
        let instance = createInstance l i 0
        if (checkIfPossible instance g 0) then acc<-acc+1
    acc

List.map2 (fun y x -> checkAllCombinations x y) springs groups 
|> List.sum 
|> printfn "Sum of combinations: %i"