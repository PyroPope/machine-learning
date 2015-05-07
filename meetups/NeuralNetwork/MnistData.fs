module MnistData

open System.IO

open Persistence

let readRows fileName =
    let file = dataDir + @"\" + fileName
    (File.ReadAllLines file).[1..] 
        |> Array.map (fun line -> line.Split(',')) 
        |> Array.map (fun fields -> fields |> Array.toList |> List.map (float))  
        

let mnistTraining =
    let buildAnswerList count index  =
        [for i in 0.0..(count - 1.0) -> if i = index then 1.0 else 0.0]
    let trainValues = readRows @"training.csv"
    let availableSamples =            
            readRows @"training.csv"
            |> Array.map (fun fields ->
            (fields.Tail, (buildAnswerList 10.0 fields.Head)))
    let availableCount = availableSamples.Length 
    let testingCount = availableCount / 10
    let trainingCount = availableCount - testingCount    
    (availableSamples.[..trainingCount-1], availableSamples.[trainingCount..])

let mnistTesting = readRows @"testing.csv" |> Seq.ofArray




