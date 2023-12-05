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
type range =
    val source:bigint
    val length:bigint
    new(x,y) =
        {
            source = x;
            length = y
        }
let lines = File.ReadLines("Input.txt")

let seedIn =
    (lines.First().Split([|':'|])[1]).Split([|' '|], StringSplitOptions.RemoveEmptyEntries) 
    |> Array.toList 
    |> List.map (fun x -> bigint(x |> int64))
let rec seeds (l:bigint list) =
    match l with 
    | x::xs -> new range(x, xs.Head)::seeds xs.Tail
    | [] -> []
let seedRanges = seeds seedIn

let map l =
    lines.SkipWhile(fun x -> x <> l)
        .Skip(1)
        .TakeWhile(fun x -> x <> "").ToList() 
        |> Seq.toList
        |> List.map (fun x -> x.Split([|' '|], StringSplitOptions.RemoveEmptyEntries))
        |> List.map (fun x -> new mapping(bigint(x[1] |> int64), bigint(x[0] |> int64), bigint(x[2] |> int64)))
        |> List.sortBy (fun x -> x.source)
// let printSeq (l:mapping list) =
//     l |> List.iter (fun x -> x.source |> printf "%A\n") 

let rec getDestination (l:mapping list) (value:range):(range list) = 
    match l with
    | x::xs -> 
        if value.source < x.source then 
            if value.source + value.length < x.source then value::[]
            else 
                if value.source + value.length < x.source + x.range then new range(value.source,x.source - value.source)::new range(x.destination, value.length + value.source - x.source)::[]
                else new range(value.source, x.source - value.source)::new range (x.destination - x.source + value.source, x.range)::getDestination xs (new range(x.source+x.range, value.length - x.range))
        else
            if value.source < x.source + x.range then 
                if value.source - x.source + value.length < x.range then (new range(x.destination - x.source + value.source, value.length))::[]
                else new range(x.destination - x.source + value.source, x.range - x.source + value.source)::getDestination xs (new range (x.source + x.range, value.length - x.range + value.source - x.source - bigint(1)))
            else getDestination xs value
    | [] -> value::[]

let seedToSoilMap = map "seed-to-soil map:"
let soilToFertilizerMap = map "soil-to-fertilizer map:"
let fertilizerToWaterMap = map "fertilizer-to-water map:"
let waterToLightMap = map "water-to-light map:"
let lightToTemperatureMap = map "light-to-temperature map:"
let temperatureToHumidityMap = map "temperature-to-humidity map:"
let humidityToLocationMap = map "humidity-to-location map:"

let getLocation (seedRanges:range list):(range list) = 
    seedRanges 
        |> List.map (fun x -> getDestination seedToSoilMap x) |> List.collect id
        |> List.map (fun x -> getDestination soilToFertilizerMap x) |> List.collect id
        |> List.map (fun x -> getDestination fertilizerToWaterMap x) |> List.collect id
        |> List.map (fun x -> getDestination waterToLightMap x) |> List.collect id
        |> List.map (fun x -> getDestination lightToTemperatureMap x) |> List.collect id
        |> List.map (fun x -> getDestination temperatureToHumidityMap x) |> List.collect id
        |> List.map (fun x -> getDestination humidityToLocationMap x) |> List.collect id
printf "Hello\n"
getLocation seedRanges 
    |> List.sortBy (fun x -> x.source)
    |> List.iter (fun x -> printf "print location ranges : s: %A r: %A\n" x.source x.length)