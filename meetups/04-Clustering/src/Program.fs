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


    let getMinsRangesAndMaxsOfDimensions vectors = 
        let first = Seq.head vectors
        let mins = Array.copy first
        let maxs = Array.copy first
        vectors
        |> Seq.iter (Array.iteri (fun i value ->
            mins.[i] <- min mins.[i] value
            maxs.[i] <- max maxs.[i] value ))            
        let ranges = (maxs, mins) ||> Array.map2 (-)
        mins, ranges, maxs
        
    let getRandomCentroids count mins ranges =
        let rnd = new Random()
        let getCentroid() = 
            (ranges,  mins)
            ||> Array.map2 (fun dif min -> rnd.NextDouble() * dif + min)
        let centroids = [| for i in 1..count ->  getCentroid() |]
        centroids

    let getVeryDifferentCentroids centroids mins maxs=
        centroids |> Array.map (fun centroid ->
            (centroid, mins, maxs) |||> Array.map3 (fun value min max ->
                if value - min > max - value then min else max ))

    let classCount = 
        samples 
        |> Array.distinctBy (fun iris -> iris.Class) 
        |> Array.length
    

    let distance A B =
        (0., A, B) 
        |||> Array.fold2 (fun sos a b -> sos + (b - a) ** 2.)
        |> (sqrt)

    let selectNearestPoint origin points =
        points |> Array.minBy (distance origin)

    let getNewClusters samples centroids = 
        samples |> Array.groupBy (fun sample -> selectNearestPoint sample.Vector centroids) 

    let getNewCentroid cluster =
        let oldCentroid , samples  = cluster       
        let vectors = samples |> Array.map(fun sample -> sample.Vector)
        vectors 
            |> Array.reduce (Array.map2 (+)) 
            |> Array.map (fun x -> x / float samples.Length)

    let doneThreshold = 0.001;
    let checkKeepGoing (centroids : float [][]) (newCentroids : float[][]) dimensionRanges =
        (centroids, newCentroids)
        ||> Seq.map2 (Seq.map2 (-))
        |> Seq.map (Seq.map2 (fun range delta -> abs (delta / range)) dimensionRanges)
        |> Seq.collect (id)
        |> Seq.exists (fun delta -> delta >= doneThreshold)
       

    let displayCluster cluster =
        printfn ""
        printfn "Centroid: "// (fst cluster)
        snd cluster 
        |> Array.groupBy (fun iris -> iris.Class)
        |> Array.sortBy (fun grp -> -(snd grp).Length)
        |> Array.iter (fun grp ->
            printf "%s" ((fst grp).PadRight(18))
            printfn "%d" (snd grp |> Array.length) )


    let dimMins, dimRanges, dimMaxs = getMinsRangesAndMaxsOfDimensions vectors
    let randomCentroids = getRandomCentroids classCount dimMins dimMaxs

    let mutable keepGoing = true
    let mutable centroids = randomCentroids
    let mutable count = 0

    while keepGoing do
        printfn ""
        let clusters  = getNewClusters samples centroids
        clusters |> Array.iter displayCluster
        let newCentroids = clusters |> Array.map getNewCentroid
        keepGoing <- checkKeepGoing centroids newCentroids dimRanges
        centroids <- newCentroids
        count <- count + 1
        printfn "Completed cycle:  %d" count
        printfn "===================="

    printfn "All done."; System.Console.ReadKey() |> ignore
    0

//let mutable keepGoing = true
//let mutable centroids = randomCentroids
//let mutable count = 0
//
//while keepGoing do
//    let clusters = samples |> Array.groupBy (fun sample -> selectNearestPoint sample.Vector centroids)   
//    printfn ""
//    clusters |> Array.iter displayCluster
//    let newCentroids = clusters |> Array.map getNewCentroid
//    keepGoing <- checkKeepGoing centroids newCentroids dimensionRanges
//    centroids <- newCentroids
//    count <- count + 1
//    printfn "Completed cycle:  %d" count
//    printfn "===================="
//
//printfn "All done."; System.Console.ReadKey() |> ignore
//0

