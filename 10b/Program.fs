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

let WriteAllLinesNoFinalLineEnd outFileName (lines : seq<string>) =
    let count = lines |> Seq.length
    use writer = new StreamWriter(outFileName, false)
    lines
    |> Seq.iteri (fun i line ->
        if i < count - 1 then
            writer.WriteLine(line)
        else
            writer.Write(line))

//Possible values : J, F, L, 7, -, |
let lookup = dict [
    "J0", Direction.left;
    "J3", Direction.up;
    "F1", Direction.down;
    "F2", Direction.right;
    "L0", Direction.right;
    "L1", Direction.up;
    "72", Direction.left;
    "73", Direction.down;
    "-1", Direction.left;
    "-3", Direction.right;
    "|0", Direction.down;
    "|2", Direction.up
]
let loop = new List<Vector2>()

let mutable board = File.ReadAllLines("Input.txt") |> Array.map (fun x -> x.ToCharArray())
let width = board[0].Length
let height = board.Length

let startingPosIndex = Array.FindIndex((board |> Array.collect id), (fun x->x='S'))
let startingPos = new Vector2(startingPosIndex%width,startingPosIndex/width)

let rec traverse x y (from:int) (distance:int) =
    loop.Add(new Vector2(x, y))
    if board[y][x] = 'S' 
        then distance
    else
        match lookup.Item((string(board[y][x]) + string from)) with
        | Direction.up    -> traverse  x   (y-1) 2 (distance+1)
        | Direction.right -> traverse (x+1) y    3 (distance+1)
        | Direction.down  -> traverse  x   (y+1) 0 (distance+1)
        | Direction.left  -> traverse (x-1) y    1 (distance+1)
        | _ -> -1 //should not happen

printfn "distance %i" ((traverse (startingPos.x) (startingPos.y+1) 0 1))
let fieldsInLoop = 
    let mutable sum = 0
    for i in 0..(height-1) do
        let mutable crosses = 0 
        let mutable crossed0 = false
        let mutable crossed1 = false
        for j in 0..(width-1) do
            if loop.Any((fun x -> x.x = j && x.y = i)) then
                match board[i][j] with
                | 'F' -> crossed0 <- true
                | 'L' -> crossed1 <- true
                | '|' -> crosses <- crosses + 1
                | 'S' -> crosses <- crosses + 1
                | 'J' -> 
                    if crossed0 then 
                        crosses <- crosses + 1
                        crossed0 <- false
                    else 
                        crossed1 <- false
                | '7' ->
                    if crossed1 then 
                        crosses <- crosses + 1
                        crossed1 <- false
                    else crossed0 <- false
                | _ -> ()
            else
                if crosses % 2 = 1 then 
                    board[i][j] <- 'O'
                    sum <- sum + 1
                else 
                    board[i][j] <- ' '
    sum
printfn "sum %i" fieldsInLoop
// WriteAllLinesNoFinalLineEnd "Out2.txt" (board |> Seq.map (fun x -> System.String x))