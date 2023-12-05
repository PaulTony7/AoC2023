open System.IO
open System
open System.Linq
type mapping =
    val source:bigint
    val destination:bigint
    val range:bigint
    new(x,y,z) =
        {
            source = x;
            destination = y;
            range = z
        }
let lines = File.ReadLines("C:\Users\Paul\Documents\GitHub\AoC2023\5a\Input.txt")

let seeds =
    (lines.First().Split([|':'|])[1]).Split([|' '|], StringSplitOptions.RemoveEmptyEntries) 
    |> Array.toList 
    |> List.map (fun x -> bigint(x |> int64))

let map l =
    lines.SkipWhile(fun x -> x <> l)
        .Skip(1)
        .TakeWhile(fun x -> x <> "").ToList() 
        |> Seq.toList
        |> List.map (fun x -> x.Split([|' '|], StringSplitOptions.RemoveEmptyEntries))
        |> List.map (fun x -> new mapping(bigint(x[1] |> int64), bigint(x[0] |> int64), bigint(x[2] |> int64)))
let printSeq (l:string list) =
    l |> List.iter (fun x -> printf "%s\n" x)

let rec getDestination (l:mapping list) (value:bigint):bigint = 
    match l with
    | x::xs -> 
        if x.source <= value && x.source + x.range > value 
            then value - x.source + x.destination 
            else getDestination xs value 
    | [] -> value
let seedToSoilMap = map "seed-to-soil map:"
let soilToFertilizerMap = map "soil-to-fertilizer map:"
let fertilizerToWaterMap = map "fertilizer-to-water map:"
let waterToLightMap = map "water-to-light map:"
let lightToTemperatureMap = map "light-to-temperature map:"
let temperatureToHumidityMap = map "temperature-to-humidity map:"
let humidityToLocationMap = map "humidity-to-location map:"
let getLocation (seed:bigint):bigint = 
    getDestination seedToSoilMap seed
        |> getDestination soilToFertilizerMap
        |> getDestination fertilizerToWaterMap
        |> getDestination waterToLightMap
        |> getDestination lightToTemperatureMap
        |> getDestination temperatureToHumidityMap
        |> getDestination humidityToLocationMap
seeds 
    |> List.map (fun x -> (getLocation x)) 
    |> List.min
    |> printf "Min seed location : %A \n"