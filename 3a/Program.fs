open System
open System.IO
open System.Linq

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

let readLines x =
    File.ReadAllLines(x) |> Array.toList
let linelength x = File.ReadLines(x).First().Length 
let fakeLine = String.replicate (linelength "Input.txt") "."
printf "%i" fakeLine.Length

let lineList = 
    (fakeLine::((readLines "Input.txt"))@[fakeLine])
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
    ['$';'*';'+';'#';'@';'-';'%';'=';'/';'&'] |> List.contains x
let isPartValid (p:part) (lines:string list):bool =
       String.exists characterIsSymbol ((lines[p.ycordinate-1])[(max 0 p.xcordinate-1)..(min (fakeLine.Length-1) (p.xcordinate+p.length))]) 
    || String.exists characterIsSymbol ((lines[p.ycordinate  ])[(max 0 p.xcordinate-1)..(min (fakeLine.Length-1) (p.xcordinate+p.length))]) 
    || String.exists characterIsSymbol ((lines[p.ycordinate+1])[(max 0 p.xcordinate-1)..(min (fakeLine.Length-1) (p.xcordinate+p.length))]) 
    
//get all parts sum
lineList 
    |> List.mapi (fun i x -> (findPartsInLine x i)) 
    |> List.collect id
    |> List.filter (fun y-> isPartValid y lineList)
    |> List.sumBy (fun x -> x.value) |> printf "sum %i\n" 