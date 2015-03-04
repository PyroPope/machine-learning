open System
open System.IO

type datum = {age:float; height:float}

let linearGuess (data: datum array) =
    let first = data.[0]
    let last = data.[data.Length - 1]
    if first.height = last.height then
        (0.0, first.height)
    else
        let a = (last.height - first.height)/(last.age - first.age)
        let c = first.height - (first.age * a)
        (a, c)            

let costOfDatum a c datum  =
    let error = datum.height - (a * datum.age + c)
    error * error

let costOfData data a c =
    data |> Array.sumBy (costOfDatum a c)

let rec climbHill func x y step = seq{
    yield (x, y, step)

    let best = 
        [(x+step, y); (x, y+step); (x-step, y); (x, y-step)]
        |> List.map (fun (x, y) -> (x, y, func x y))
        |> List.sortBy (fun (_, _, value) -> - value)
        |> List.head

    yield! match best with
            | (x', y', value') when value' > func x y -> climbHill func x' y' step
            | (_, _, value')  -> climbHill func x y (step/10.0)
}    

[<EntryPoint>]
let main argv = 
    Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

    let readFloats file =  
        File.ReadAllLines file
        |> Array.map float

    let sampleData = 
        Array.zip (readFloats @"..\data\ex2x.dat") (readFloats @"..\data\ex2y.dat") 
        |> Array.map(fun (a, h) -> {age=a; height = h})
        
    let step = 1.0;
    let aGuess, cGuess = linearGuess(sampleData) 
    let quality a c = -(costOfData sampleData a c)    
    let trail = climbHill quality aGuess cGuess step
   
    let a, c, _ = 
        trail 
        |> Seq.where (fun (_, _, step) -> abs step < 0.00000000001)
        |> Seq.head

    printfn "a:%0.10f c:%0.10f" a c 
    
    Console.ReadKey() |> ignore
    0
