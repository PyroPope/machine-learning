module Training
open System
open ANN
open Persistence
open Primes

type SessionStats =
    {   start : DateTime
        duration : TimeSpan
        lastCost: float
        costReduction : float
        cycleCount : int
    }

type CycleResult =
    {   net : float list list list
        cost : float
        sampleCount :int
        correctCount : int
        startTime : DateTime
        duration : TimeSpan
        lastResult : BackPropagationResult 
        testNetResult : int * int
        stats : SessionStats}

let now() = DateTime.UtcNow



let display r =
    printfn "Completed cycle %d" r.stats.cycleCount
    printfn "  cost:                   %.15f" r.cost
    let sampleCount = r.sampleCount
    let correctPercent = 100.0 * float r.correctCount / float sampleCount
    printfn "  correct:                %.2f%% (%d of %d)" correctPercent r.correctCount sampleCount
    let testCorrect, testCount = r.testNetResult
    let testPercent = 100.0 * (float testCorrect) / float testCount
    printfn "  test:          %.2f%%" testPercent
    printfn "  cycle time:    %s" (r.duration.ToString("hh\:mm\:ss"))
    printfn "  run time:      %s" (r.stats.duration.ToString("hh\:mm\:ss"))
    let makeBig float = 10.**10. * float
    let costRed = (makeBig r.stats.costReduction)
    let pad = if costRed >= 0. then " " else ""
    printfn "  reduction<10: %s%.2f" pad costRed 
    printfn ""
    writeLog [sampleCount; r.correctCount; r.cost.ToString("0.000000000000000"); testPercent.ToString("00.000"); costRed ]

let trainSample net learnRate sample =    
    backPropagate net learnRate sample
    
//let calcCost results = 
//    let errorsSquared = 
//        results 
//        |> List.map (fun r ->
//            (0., r.output, r.sample.target) 
//            |||> List.fold2 (fun s o t -> s + (t - o)**2.))
//        |> List.sum
//    errorsSquared / (2. * float results.Length)

let emptyBPResult = {sample = {input=[]; target=[]}; output = []; newNet = []}

let private trainCycle net learnRate samples checkCorrect stats testNet =
    let start = now()

    let results = samples |> Seq.map (trainSample net learnRate)
    let count, correctCount, errSqSum, lastResult = ((0, 0, 0.0, emptyBPResult), Seq.zip (Seq.ofList samples) results) ||> Seq.fold (fun (count, correctCount, errSqSum, lastResult) (sample, result)->
        let errorSquare = (0., Seq.zip sample.target result.output) ||> Seq.fold (fun sum (t, o) -> sum + (t - o)**2.) 
        let newCorrectCount = if checkCorrect result then correctCount + 1 else correctCount    
        (count + 1, newCorrectCount, errSqSum + errorSquare, result ))

    let sampleCount = count
    let cost = errSqSum / (2. * float count)
    let costReduction = stats.lastCost - cost
    let lastResult = lastResult
    {   net = lastResult.newNet
        cost = cost
        sampleCount = sampleCount
        correctCount = correctCount
        startTime = start
        duration = now() - start
        lastResult = lastResult 
        testNetResult = testNet lastResult.newNet sampleCount
        stats = 
        { stats with 
            duration = stats.start - now()
            cycleCount = stats.cycleCount + 1
            lastCost = cost
            costReduction = costReduction
            }}

let private createStats samplesLength = {
        start = now()
        duration = TimeSpan.Zero
        cycleCount = 0  
        lastCost = 0.
        costReduction = 2.
        }

let trainSeries net learnRate samples checkCorrect testNet =
    trainCycle net  learnRate samples checkCorrect (createStats samples.Length) testNet

let getTrainingSetProvider samples =
    let size = Array.length samples
    let getIndexes =
        let primes = getPrimes (int (sqrt (float size))) (Int32.MaxValue / size)
        fun () -> 
            let prime = primes.[rnd.Next(primes.Length)]
            [1..size] |> List.map (fun i -> (i * prime) % size)    
    fun () -> 
        getIndexes() |> List.map (fun i -> samples.[i])          

let private trainUntilWithStats net learnRate samples checkCorrect checkDone stats testNet =
    let trainingSetProvider = getTrainingSetProvider samples
    let rec runAndCheck net stats =
        let r = trainCycle net learnRate (trainingSetProvider()) checkCorrect stats testNet
        display r
        match checkDone r with
        | true -> r
        | false -> runAndCheck r.net r.stats
    runAndCheck net stats

let trainUntil net learnRate samples checkCorrect checkDone =
     trainUntilWithStats net learnRate samples checkCorrect checkDone (createStats samples.Length)

let trainIncrementally net learnRate samples checkCorrect checkDone start testNet onIncrement =
    let stats = createStats (Array.length samples)
    let rec trainIncrementallyWithStats net learnRate samples start  stats =
        let samplesLength = Array.length samples
        match start >= samplesLength with
        | false ->
            let r = trainUntilWithStats net learnRate samples.[1..start] checkCorrect checkDone stats testNet
            let newStart = start + max 1 (start / 13)
            onIncrement start newStart r.net
            trainIncrementallyWithStats  r.net learnRate samples  newStart   stats
        | true ->
            trainUntilWithStats net learnRate samples checkCorrect checkDone stats testNet
    let x = trainIncrementallyWithStats net learnRate samples  start  stats
    x
