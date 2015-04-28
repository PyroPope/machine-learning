module Mnist

open ANN
open Training
open Persistence

let mnist trainSize =
    let stateFile = "mnist"
    let learnRate = 0.7

    printfn "Using state file: \"%s\"" stateFile
    let net = 
        match tryLoad stateFile with 
        // http://yann.lecun.com/exdb/mnist/  -> 500-100 
        // (wiki MNIST database) -> 784 [2500; 2000; 1500; 1000; 500] 10 
        | None -> printfn "Creating NEW net"; createNet [784; 500; 100; 10]
        | Some(net) -> printfn "Loading existing net"; net

    printf "Loading samples: "
    let tup2Sample tuples = 
        tuples 
        |> Array.map (fun (input, target) -> {input=input; target=target})

    let mnistTups = MnistData.mnistData
    let trainingSamples = tup2Sample (fst mnistTups)
    printf "%d training, " trainingSamples.Length
    let testingSamples = tup2Sample (snd mnistTups)
    printfn "%d testing" testingSamples.Length
    let testingSamplesCount = testingSamples.Length

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
        cycleResult.cost < 0.0001 && cycleResult.correctCount = cycleResult.sampleCount 

    let testNet net sampleCount =
        let testCount = max 10 (min sampleCount testingSamplesCount)
        save stateFile net
        let correctCount =
            testingSamples.[0..(testCount - 1)]
            |> Array.map (fun s ->  
                checkCorrect s.target (feedForward net learnRate s.input))
            |> Array.filter (fun b -> b)
            |> Array.length
        (correctCount, testCount)

    let primePump = testNet net 1
    let initialAccuracy size label = 
        printf "Initial accuracy, %s: " label
        let start = now()
        let right, count = testNet net size
        let duration = now() - start
        let correctPercent = 100.0 * float right / float count
        printfn " %.2f%% %d/%d in %.1fs" correctPercent right count duration.TotalSeconds
    initialAccuracy testingSamplesCount "all"
    initialAccuracy trainSize "stats"


    let onIncrement start newStart net =
        let startFile = sprintf "%s-%s" stateFile (start.ToString().PadLeft(5, '0'))
        save startFile net
        let newStartFile = sprintf "%s-%s" stateFile (newStart.ToString().PadLeft(5, '0'))
        save newStartFile net

    printfn "Starting first cycle..."
    printfn ""

    trainIncrementally net learnRate trainingSamples checkResult checkDone trainSize testNet onIncrement
    |> ignore
//    let jobTrainSamples = trainingSamples.[..(trainSize - 1)]
//    trainUntil net learnRate jobTrainSamples checkResult checkDone testNet |> ignore
    ()