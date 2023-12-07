open System.IO
open System
open System.Collections.Generic


type Hand =
    val cards:(int list)
    val strength:int
    val bid:int
    new(x,y,z) =
        {
            cards = x;
            strength = y;
            bid = z
        }

let lines = File.ReadAllLines("Input.txt") |> Array.toList
let hands = lines |> List.map (fun x -> x.Split([|' '|])[0])
let bids = lines |> List.map (fun x -> (x.Split([|' '|])[1]) |> int)

let readCard (c:char) =
    match c with 
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 1
    | 'T' -> 10
    | _ -> int c - int '0'

let rec HighCardStrength (l:int list) =
    match l with 
    | x::xs -> x + 100*(HighCardStrength xs)
    | _ -> 0
let rec parseHand (h:char list) (d: Dictionary<int,int>) =
    match h with
    | x::xs -> 
        let c = readCard x
        if d.ContainsKey(c) then
            d[c]<-d[c] + 1
            parseHand xs d
        else 
            d.Add(c, 1)
            parseHand xs d
    | [] -> ()

let calcStrength (d:int list) =
    match d[0] with
        | 5 -> 7
        | 4 -> 6
        | 3 -> if d[1] = 2 then 5 else 4
        | 2 -> if d[1] = 2 then 3 else 2
        | _ -> 1
let calcStrengthWithJokers (jokers:int)  (d:int list) =
    if jokers = 3 then
        match d[0] with
        | 2 -> 7
        | 1 -> 6
        | _ -> 1
    else
        if jokers = 2 then
            match d[0] with
            | 3 -> 7
            | 2 -> 6
            | 1 -> 4
            | _ -> 1
        else
            if jokers = 1 then
                match d[0] with
                | 4 -> 7
                | 3 -> 6
                | 2 -> if d[1] = 2 then 5 else 4
                | 1 -> 2
                | _ -> 1
            else calcStrength d

let createHand (h:char list) (b:int) = 
    let d = new Dictionary<int, int>() 
    parseHand h d
    let readHand = h |> List.map (fun x -> readCard x)
    let jokers = if d.ContainsKey(1) then d[1] else 0
    if jokers >= 4 then 
        new Hand(readHand, 7, b)
    else
        let calcJokerStrength = calcStrengthWithJokers jokers
        let strength  = 
            d 
            |> Seq.filter (fun x -> x.Key <> 1) 
            |> Seq.map (fun x -> x.Value) 
            |> Seq.toList
            |> List.sortDescending 
            |> calcJokerStrength
        new Hand(readHand, strength, b)

let parsedHands = List.map2 (fun x y -> createHand (Seq.toList x) y) hands bids 

parsedHands 
|> List.sortBy (fun x -> HighCardStrength (x.cards |> List.rev))  
|> List.sortBy (fun x -> x.strength)  
|> List.mapi (fun i x -> x.bid * (i+1))
|> List.sum 
|> printf "Total sum %i\n"
