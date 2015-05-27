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
        let vector = [| for i in 0..3 -> float row.[i] |]
        {   Vector = vector;
            SepalLength = vector.[0]
            SepalWidth = vector.[1]
            PetalLength = vector.[2]
            PetalWidth = vector.[3]
            Class = row.[4] })
    
    let vectors = [| for sample in samples -> sample.Vector |]


    let getMinsAndMaxsOfDimensins vectors = 
        let first = Seq.head vectors
        let mins = Array.copy first
        let maxs = Array.copy first
        vectors
        |> Seq.iter (Array.iteri (fun i value ->
            mins.[i] <- min mins.[i] value
            maxs.[i] <- max maxs.[i] value ))            
        mins, maxs
        
    let getCentroids count vectors =
        let mins, maxs = getMinsAndMaxsOfDimensins vectors  
        let difs = (maxs, mins) ||> Array.map2 (-)
        let rnd = new Random()
        let getCentroid() = 
            (difs, mins)
            ||> Array.map2 (fun dif min -> rnd.NextDouble() * dif + min)
        [| for i in 1..count ->  getCentroid() |]

    let classCount = 
        samples 
        |> Array.distinctBy (fun iris -> iris.Class) 
        |> Array.length
    
    let centroids = getCentroids classCount vectors
    printfn "%A" centroids

    let distance A B =
        (0., A, B) 
        |||> Array.fold2 (fun sos a b -> sos + (b - a) ** 2.)
        |> (sqrt)

    let selectNearestPoint points origin  =
        points |> Array.minBy (distance origin)
    
    let clusters = vectors |> Array.groupBy (selectNearestPoint centroids)   
    
    printfn "%A" clusters

    printfn "All done."; System.Console.ReadKey() |> ignore
    0

