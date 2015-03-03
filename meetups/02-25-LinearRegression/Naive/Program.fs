// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System
open System.IO

let appDir =  DirectoryInfo( __SOURCE_DIRECTORY__).Parent
let dataDir = Path.Combine(appDir.FullName, "Data")
    
let ageFile = Path.Combine(dataDir, "ex2x.dat")
let heightFile = Path.Combine(dataDir, "ex2x.dat")

printfn "%s" ageFile

let readLines (filePath:string) =  [
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        let line = sr.ReadLine ()
        yield Double.Parse(line)
]

let ages = readLines ageFile
let heights = readLines heightFile

type datum = {age:float; height:float}

printfn "got %d ages and %d heights" ages.Length heights.Length

let sampleData = List.zip ages heights |> List.map(fun (a, h) -> {age=a; height = h})

let rec climbHill (f: float -> float) (x:float) (d:float) = seq{   
    let y = f x
    yield (x, y)
    let x' = x + d
    let y' = f x'
    let d' = if y' > y then d else -d/2.0
    yield! climbHill f x' d'       
}    

for d in sampleData do
    printfn "age:%f height:%f" d.age d.height

let expected a c x = a*x + c

let costDatum a c (dtm:datum)  =
    let exp = expected a c dtm.age
    let dist = exp - dtm.height
    dist * dist

let cost (data:datum list) a c =
    data 
    |> List.map (costDatum a c)
    |> List.sum

let ourF a = -(cost sampleData a 0.75)
    
let trail = climbHill ourF 1.0 1.0

for (aa, cc) in trail do
    printfn "a:%f cost:%f" aa -cc

[<EntryPoint>]
let main argv = 
    0
