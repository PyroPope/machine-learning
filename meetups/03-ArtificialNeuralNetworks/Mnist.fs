module Mnist



open ANN
open Training
open Persistence

let mnist trainSize =
    let stateFile = "mnist"
    let learnRate = 0.8

    printfn "Using state file: \"%s\"" stateFile
    let net = 
        match tryLoad stateFile with 
        // http://yann.lecun.com/exdb/mnist/  -> 500-100 
        // (wiki MNIST database) -> 784 [2500; 2000; 1500; 1000; 500] 10 
        | None -> printf "Creating NEW net: "; createNet [784; 500; 100; 10]
        | Some(net) -> printf "Loading existing net: "; net

    let displayNetInfo =
        let inputSize = net.Head.Head.Length - 1
        let sizes = (net |> List.map (List.length))
        let sizesString = 
            inputSize :: sizes
            |> List.map (fun i -> i.ToString("n0"))
            |> String.concat "-"
        let connectionCount = 
            ((0, inputSize), sizes)
            ||> List.scan (fun (_, previous) current -> (previous + 1, current))
            |> List.tail
            |> List.map (fun (a, b) -> a*b)
            |> List.sum
            
        printfn "[%s], %s connections" sizesString (connectionCount.ToString("n0"))


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
//        let newCheck cr =
//            cr.stats.costReduction > 0.
//            && cr.stats.costReduction < 0.0000000150 
//            && cr.correctCount >= ((cr.sampleCount *12) / 13)
//        printfn "  newTest: %b" (newCheck cycleResult)
        let reduction = cycleResult.stats.costReduction
        cycleResult.correctCount >= ((cycleResult.sampleCount *12) / 13)
            && reduction > 0.            
            && (cycleResult.cost < 0.0001
                || (cycleResult.cost < 0.01 
                    && cycleResult.stats.costReduction < 0.0000000150 ))

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
    printfn "Learning Rate: %.2f" learnRate


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