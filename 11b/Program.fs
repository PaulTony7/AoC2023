open System.IO
open System.Linq

type Vector2 =
    val x:int;
    val y:int;
    new (x, y) =
        {
            x=x;
            y=y;
        }
let distanceMul = 2
let lines = 
    File.ReadAllLines("C:\Users\kokot.p\Documents\GitHub\AoC2023\11b\Input.txt") 
    |> Array.toList 
    |> List.map (fun x -> x.ToCharArray() |> Array.toList)
// List.iter (fun x -> printfn "%s" x) lines
let lineEmpty l =
    List.forall (fun x -> x = '.') l
let rec findHorizontalLines i (l)  =
    match l with
    | x::xs -> 
        if (lineEmpty x) then i::findHorizontalLines (i+1) xs 
        else findHorizontalLines (i+1) xs 
    | _ -> []
let rec verticalSlice (l: char list list) (i:int) =
    match l with
    | x::xs -> x[i]::verticalSlice xs i
    | _ -> []
let rec rotate90 (l: char list list) =
    List.mapi (fun i x -> verticalSlice l i) l[0]
let implode (xs:char list) =
        let sb = System.Text.StringBuilder(xs.Length)
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString()

let horizontalLines = 
    findHorizontalLines 0 lines

let verticalLines =
    rotate90 lines 
    |> findHorizontalLines 0 
    // |> rotate90 
let rec findGalaxies l i j =
    let j2 = horizontalLines |> List.filter (fun x-> x < j)
    let j2 = j2.Length*(distanceMul-1)+j
    match l with
    | x::xs -> 
        if x = '#' then new Vector2(i,j2)::findGalaxies xs (i+1) j2
        else 
            if verticalLines.Any((fun x-> x = i)) then findGalaxies xs (i+distanceMul) j2
            else findGalaxies xs (i+1) j2
    | _ -> []
let galaxies = 
    lines 
    |> List.mapi (fun j x -> (findGalaxies x 0 j)) 
    |> List.collect id

let distance (x:Vector2) (y:Vector2) =
    abs (x.x-y.x)+abs(x.y-y.y)

let rec calculateDistance (l:Vector2 list) =
    match l with 
    | x::xs -> (l |> List.fold (fun acc y -> acc + (distance x y)) 0 )::(calculateDistance xs)
    | _ -> []
let calcD = calculateDistance galaxies |> List.sum
printfn "Total distance sum %i" calcD
