// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
// y=0.06391x + 0.750029999999

open System
open System.IO

let readLines (filePath:string) =  [
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        let line = sr.ReadLine ()
        yield Double.Parse(line)
]

type datum = {age:float; height:float}

let rec climbHill (f: float -> float) (x:float) (step:float) = seq{   
    let y = f x
    yield (x, y)
    let x' = x + step
    let y' = f x'
    let step' = if y' > y then step else -step/2.0
    yield! climbHill f x' step'       
}    

let estimate a c x = a*x + c

let costDatum a c (dtm:datum)  =
    let est = estimate a c dtm.age
    let error = est - dtm.height
    error * error

let cost (data:datum list) a c =
    data 
    |> List.map (costDatum a c)
    |> List.sum

[<EntryPoint>]
let main argv = 
    let appDir =  DirectoryInfo( __SOURCE_DIRECTORY__).Parent
    let dataDir = Path.Combine(appDir.FullName, "Data")
    
    let ageFile = Path.Combine(dataDir, "ex2x.dat")
    let heightFile = Path.Combine(dataDir, "ex2y.dat")

    printfn "%s" ageFile

    let ages = readLines ageFile
    let heights = readLines heightFile

    printfn "got %d ages and %d heights" ages.Length heights.Length

    let sampleData = List.zip ages heights |> List.map(fun (a, h) -> {age=a; height = h})
    
    for d in sampleData do
        printfn "age:%f height:%f" d.age d.height
    
    let c = 0.75003
    let ourF a = -(cost sampleData a c)
    
    let trail = climbHill ourF 1.0 10.9

    for (a, cost) in trail do
        printfn "a:%f cost:%f" a cost

    0
