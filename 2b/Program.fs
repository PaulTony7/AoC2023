//12 red cubes, 13 green cube, 14 blue cubes
open System.IO

let max_r = 12
let max_g = 13
let max_b = 14

let getValue (x:string):int = 
    (x.Split[|' '|])[1] |> int

let checkIfPossible (r, g, b) =
    r <= max_r && g <= max_g && b <= max_b

let getPower (r, g, b) = r * g * b

let getLineValues (x:string) =
    let mutable r = 0
    let mutable g = 0
    let mutable b = 0
    let l = (x.Split[|',';';'|]) 
    for item in l do
        let itemSplit = item.Split[|' '|]
        match itemSplit.[2] with
        | "red" -> r <- max r (itemSplit.[1] |> int)
        | "blue" -> b <- max b (itemSplit.[1] |> int)
        | "green" -> g <- max g (itemSplit.[1] |> int)
        | _ -> ()
    (r, g, b)

let readLines x =
    File.ReadAllLines(x) |> Array.toList
    |> List.map (fun x -> x.Split[|':'|] )
    // |> List.filter (fun x -> x.[1] |> getLineValues |> checkIfPossible)
    |> List.sumBy (fun x -> x.[1] |> getLineValues |> getPower)

printfn "Sum all indices: %i" (readLines "Input.txt")