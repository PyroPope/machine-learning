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

let linearGuess (data: datum list) =
    let first = data.Head
    let last = List.reduce (fun _ i -> i) data
    if first.height = last.height then
        (0.0, first.height)
    else
        let a = (last.height - first.height)/(last.age - first.age)
        let c = first.height - (first.age * a)
        (a, c)            

let costDatum a c (dtm:datum)  =
    let est = a * dtm.age + c
    let error = est - dtm.height
    error * error

let cost (data:datum list) a c =
    data 
    |> List.map (costDatum a c)
    |> List.sum

let rec climbHill (f: float -> float) (x:float) (step:float) = seq{   
    let y = f x
    yield (x, y, step)
    let x' = x + step
    let y' = f x'
    let step' = if y' > y then step else -step/10.0
    yield! climbHill f x' step'       
}    

[<EntryPoint>]
let main argv = 
    let appDir =  DirectoryInfo( __SOURCE_DIRECTORY__).Parent
    let dataDir = Path.Combine(appDir.FullName, "Data")
    
    let ageFile = Path.Combine(dataDir, "ex2x.dat")
    let heightFile = Path.Combine(dataDir, "ex2y.dat")

    printfn "%s" ageFile

    let ages = readLines ageFile
    let heights = readLines heightFile

    let sampleData = List.zip ages heights |> List.map(fun (a, h) -> {age=a; height = h})
    
    //let (aGuess, cGuess) = linearGuess(sampleData) 
    let (aGuess, cGuess) = 1.0, 0.750029999999
    
    let step = 1.0;
    
    let ourF x = -(cost sampleData x cGuess)
    
    let trail = climbHill ourF aGuess step
    
    let (a, cost, step) = 
        trail 
        |> Seq.where (fun (_, _, step) -> abs step < 0.0000001)
        |> Seq.head

    printfn "answer:%0.5f cost:%0.5f step:%0.10f" a cost step
    
    printfn "Press any key to close."
    Console.ReadKey() |> ignore
    0
