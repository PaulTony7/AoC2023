open System
open System.Linq
open System.IO
open System.Collections.Generic

type Vector2 =
    val x:int;
    val y:int;
    new (x, y) =
        {
            x=x;
            y=y;
        }

type Direction =
    | up = 0
    | right = 1
    | down = 2
    | left = 3

//Possible values : J, F, L, 7, -, |
let lookup = new Dictionary<string, Direction>()
lookup.Add("J0",Direction.left)
lookup.Add("J3",Direction.up)
lookup.Add("F1",Direction.down)
lookup.Add("F2",Direction.right)
lookup.Add("L0",Direction.right)
lookup.Add("L1",Direction.up)
lookup.Add("72",Direction.left)
lookup.Add("73",Direction.down)
lookup.Add("-1",Direction.left)
lookup.Add("-3",Direction.right)
lookup.Add("|0",Direction.down)
lookup.Add("|2",Direction.up)


let board = File.ReadAllLines("C:\Users\kokot.p\Documents\GitHub\AoC2023\10a\Input.txt") |> Array.map (fun x -> x.ToCharArray())
let width = board[0].Length
let height = board.Length

let startingPosIndex = Array.FindIndex((board |> Array.collect id), (fun x->x='S'))
let startingPos = new Vector2(startingPosIndex%width,startingPosIndex/width)

let rec traverse x y (from:int) (distance:int) =
    if board[y][x] = 'S' 
        then distance
    else
        match lookup[(string(board[y][x]) + string from)] with
        | Direction.up    -> traverse  x   (y-1) 2 (distance+1)
        | Direction.right -> traverse (x+1) y    3 (distance+1)
        | Direction.down  -> traverse  x   (y+1) 0 (distance+1)
        | Direction.left  -> traverse (x-1) y    1 (distance+1)
        | _ -> -1 //should not happen

// let scanStartingPos (v:Vector2) =
//     //Scan up
//     if ['F'; '|'; 'J'].Contains(board[v.y-1][v.x]) then
//         ()
//     //Scan right
//     else if ['J';'7';'-'].Contains(board[v.y][v.x+1]) then
//         ()
//     //traverse down
//         ()
        
// printfn "width: %i height %i" width height
// printfn "starting Pos: %i %i : %c" startingPos.x startingPos.y (board[startingPos.y][startingPos.x])
printfn "distance %i" ((traverse (startingPos.x) (startingPos.y+1) 0 1) / 2)