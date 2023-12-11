open System.IO

type Vector2 =
    val x:int;
    val y:int;
    new (x, y) =
        {
            x=x;
            y=y;
        }

let lines = 
    File.ReadAllLines("Input.txt") 
    |> Array.toList 
    |> List.map (fun x -> x.ToCharArray() |> Array.toList)
// List.iter (fun x -> printfn "%s" x) lines
let lineEmpty l =
    List.forall (fun x -> x = '.') l
let rec ExpendHorizontalLines (l) =
    match l with
    | x::xs -> 
        if (lineEmpty x) then x::x::(ExpendHorizontalLines xs)
        else x::ExpendHorizontalLines xs
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

let expendedUniverse = 
    ExpendHorizontalLines lines 
    |> rotate90 
    |> ExpendHorizontalLines 
    // |> rotate90 
let rec findGalaxies l i j =
    match l with
    | x::xs -> 
        if x = '#' then new Vector2(i,j)::findGalaxies xs (i+1) j
        else findGalaxies xs (i+1) j
    | _ -> []
let galaxies = 
    expendedUniverse 
    |> List.mapi (fun i x -> (findGalaxies x 0 i)) 
    |> List.collect id

let distance (x:Vector2) (y:Vector2) =
    abs (x.x-y.x)+abs(x.y-y.y)

let rec calculateDistance (l:Vector2 list) =
    match l with 
    | x::xs -> (l |> List.fold (fun acc y -> acc + (distance x y)) 0 )::(calculateDistance xs)
    | _ -> []
let calcD = calculateDistance galaxies |> List.sum
printfn "Total distance sum %i" calcD
