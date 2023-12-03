open System
open System.IO
open System.Linq

let inputPath = "Input.txt"
type part =
    struct
        val value: int
        val xcordinate: int
        val ycordinate: int
        val length: int
        new(v:int, x:int, y:int, l:int) = 
            { 
            value = v; 
            xcordinate = x; 
            ycordinate = y; 
            length = l
            }
    end
type gear = 
    val x:int
    val y:int
    val mutable parts: int
    val mutable times: int
    new(x:int, y:int) =
        {
            x=x;
            y=y;
            parts=1;
            times=0
        }
let readLines x =
    File.ReadAllLines(x) |> Array.toList

let linelength x = File.ReadLines(x).First().Length 

let fakeLine = String.replicate (linelength inputPath) "."

let lineList = 
    (fakeLine::((readLines inputPath))@[fakeLine])

let rec findPartsInLine (l:string) (lnumber:int) =
    if l.Length = 0 then []
    else
    if Char.IsNumber(l[0]) 
    then
        let hit = l.Split([|'.';'$';'*';'+';'#';'@';'-';'%';'=';'/';'&'|])[0]
        new part((hit |> int), fakeLine.Length-l.Length, lnumber, hit.Length)::findPartsInLine l[hit.Length..] lnumber
    else 
        findPartsInLine l[1..] lnumber

let characterIsSymbol (x:char):bool =
    ['*'] |> List.contains x 

let rec findAllGears (l:string) (lnumber:int) =
    if l.Length = 0 then []
    else
    if l[0] = '*' 
    then
        new gear(fakeLine.Length-l.Length, lnumber)::findAllGears l[1..] lnumber
    else 
        findAllGears l[1..] lnumber

let isPartGear (p:part) (lines:string list):bool =
       String.exists characterIsSymbol ((lines[p.ycordinate-1])[(max 0 p.xcordinate-1)..(min (fakeLine.Length-1) (p.xcordinate+p.length))]) 
    || String.exists characterIsSymbol ((lines[p.ycordinate  ])[(max 0 p.xcordinate-1)..(min (fakeLine.Length-1) (p.xcordinate+p.length))]) 
    || String.exists characterIsSymbol ((lines[p.ycordinate+1])[(max 0 p.xcordinate-1)..(min (fakeLine.Length-1) (p.xcordinate+p.length))]) 
    
let attachPartToGear (p:part) (lines:string list) (g:gear list)=
    g |> List.filter (fun e -> e.x >= (max 0 p.xcordinate-1) 
                                     && e.x <= (min (fakeLine.Length-1) (p.xcordinate+p.length))
                                     && e.y >= p.ycordinate-1 && e.y <= p.ycordinate+1)
    |> List.iter (fun x -> 
        x.parts <- (x.parts * p.value)
        x.times <- x.times + 1)
//get all gears
let gearList = 
    lineList 
    |> List.mapi (fun i x -> findAllGears x i)
    |> List.collect id

lineList
    |> List.mapi (fun i x -> (findPartsInLine x i)) 
    |> List.collect id 
    |> List.filter (fun y-> isPartGear y lineList)
    |> List.iter (fun x -> attachPartToGear x lineList gearList)

gearList 
    |> List.filter (fun x -> x.times = 2)
    |>  List.sumBy (fun x -> x.parts) |> printf "sum %i\n"