open System

type Iris = {
    Vector : float[]
    SepalLength : float
    SepalWidth : float
    PetalLength : float
    PetalWidth : float
    Class : string
}

[<EntryPoint>]
let main argv = 
    printfn "Hello, got args: %A" argv

    let samples =
        Data.readRows "iris.data" 
        |> Array.map (fun row -> 
        let vector = [|0..3|] |> Array.map (fun i -> float row.[i])
        {   Vector = vector;
            SepalLength = vector.[0]
            SepalWidth = vector.[1]
            PetalLength = vector.[2]
            PetalWidth = vector.[3]
            Class = row.[4] })

    let getMinsAndMaxs vectors = 
        let first = Seq.head vectors
        let mins = Array.copy first
        let maxs = Array.copy first
        vectors
        |> Seq.iter (Array.iteri (fun i v ->
            mins.[i] <- min mins.[i] v
            maxs.[i] <- max maxs.[i] v
        ))            
        mins, maxs
        
    let getCentroids count vectors =
        let mins, maxs = getMinsAndMaxs vectors  
        let difs = (maxs, mins) ||> Array.map2 (-)
        let rnd = new Random()
        let getCentroid() = 
            (difs, mins)
            ||> Array.map2 (fun dif min -> rnd.NextDouble() * dif + min)
        [|1..count|] |> Array.map (fun _ -> getCentroid())
            

    let vectors = samples |> Array.map (fun s -> s.Vector)
    let centroids = getCentroids 3 vectors
    printfn "%A" centroids

    printfn "All done."; System.Console.ReadKey() |> ignore
    0

