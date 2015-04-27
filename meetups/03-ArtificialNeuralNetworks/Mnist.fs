module Mnist

open ANN
open Training
open Persistence
open MnistData


let mnist jobTrainSize =
    let stateFile = "mnist"

    printfn "Using state file: %s" stateFile
    let net = 
        match tryLoad stateFile with 
        // http://yann.lecun.com/exdb/mnist/  -> 500-100 
        // (wiki MNIST database) -> 784 [2500; 2000; 1500; 1000; 500] 10 
        | None -> printfn "Creating NEW net"; createNet [784; 500; 100; 10]
        | Some(net) -> printfn "Loading EXISISING net"; net

    printfn "Loading samples..."
    let availableSamples = mnistData()

    let availableCount = availableSamples.Length 
    printf "Got a total of %d samples, " availableCount
    let testingCount = availableCount / 10
    let trainingCount = availableCount - testingCount
    
    let tup2rec tuples = 
        tuples 
        |> Array.map (fun (input, target) -> {input=input; target=target})
    
    let trainingSamples = tup2rec availableSamples.[..trainingCount-1]
    let testingSamples = tup2rec availableSamples.[trainingCount..]

    printfn "using %d for testing" testingCount
    
    let checkCorrect answer output=
        let maxIndex list =
            list
            |> List.mapi(fun i l -> i, l)
            |> List.maxBy snd
            |> fst
        maxIndex answer = maxIndex output

    let checkResult bpResult =
        checkCorrect  bpResult.sample.target bpResult.output

    let checkDone cycleResult = 
        cycleResult.cost < 0.005

    let jobTestSamples = 
        testingSamples.[0..999]
    let jobTestCount = jobTestSamples.Length 
    let testNet net =
        save stateFile net
        let correctCount =
            jobTestSamples 
            |> Array.map (fun s ->  
                checkCorrect s.target (feedForward net s.input))
            |> Array.filter (fun b -> b)
            |> Array.length
        (correctCount, jobTestCount)

    printfn "Starting first cycle..."
    printfn ""

    let x = trainIncrementally net trainingSamples checkResult checkDone jobTrainSize testNet
//    let jobTrainSamples = trainingSamples.[..jobTrainSize - 1]
//    let x = trainUntil net jobTrainSamples checkCorrect checkDone ()
    ()