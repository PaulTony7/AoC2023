open System.IO
open System.Linq

type Vector2 =
    val x:bigint;
    val y:bigint;
    new (x, y) =
        {
            x=x;
            y=y;
        }
let distanceMul = 1000000I
let lines = 
    File.ReadAllLines("Input.txt") 
    |> Array.toList 
    |> List.map (fun x -> x.ToCharArray() |> Array.toList)
    
let lineEmpty l =
    List.forall (fun x -> x = '.') l

let rec findHorizontalLines i (l)  =
    match l with
    | x::xs -> 
        if (lineEmpty x) then i::findHorizontalLines (i+1I) xs 
        else findHorizontalLines (i+1I) xs 
    | _ -> []

let rec verticalSlice (l: char list list) (i:int) =
    match l with
    | x::xs -> x[i]::verticalSlice xs i
    | _ -> []

let rec rotate90 (l: char list list) =
    List.mapi (fun i x -> verticalSlice l i) l[0]

let horizontalLines = 
    findHorizontalLines 0I lines

let verticalLines =
    rotate90 lines 
    |> findHorizontalLines 0I 
    
let rec findGalaxies l i i2 j =
    let j2 = horizontalLines |> List.filter (fun x-> x < j)
    let j2 = bigint j2.Length*(distanceMul-1I)+j
    match l with
    | x::xs -> 
        if x = '#' then new Vector2(i2,j2)::findGalaxies xs (i+1I) (i2+1I) j
        else 
            if verticalLines.Any((fun x-> x = i)) then findGalaxies xs (i+1I) (i2+distanceMul) j
            else findGalaxies xs (i+1I) (i2+1I) j
    | _ -> []
let galaxies = 
    lines 
    |> List.mapi (fun j x -> (findGalaxies x 0I 0I (bigint j))) 
    |> List.collect id

let distance (x:Vector2) (y:Vector2) =
    abs (x.x-y.x)+abs(x.y-y.y)

let rec calculateDistance (l:Vector2 list) =
    match l with 
    | x::xs -> (l |> List.fold (fun acc y -> acc + (distance x y)) 0I )::(calculateDistance xs)
    | _ -> []
let calcD = calculateDistance galaxies |> List.sum
printfn "Total distance sum %A" calcD
