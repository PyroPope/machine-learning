module Training
open System
open ANN
open Primes

type SessionStats =
    {   start : DateTime
        duration : TimeSpan
        totalSampleCount : int
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
    printfn "Completed cycle: %d" r.stats.cycleCount
    printfn "  cost:                   %.15f" r.cost
    let sampleCount = r.sampleCount
    let correctPercent = 100.0 * float r.correctCount / float sampleCount
    printfn "  correct:                %.2f%% (%d of %d)" correctPercent r.correctCount sampleCount
    let testCorrect, testCount = r.testNetResult
    let testPercent = 100.0 * (float testCorrect) / float testCount
    printfn "  test:          %.2f%%" testPercent
    printfn "  cycle time:    %s" (r.duration.ToString("hh\:mm\:ss"))
    printfn "  run time:      %s" (r.stats.duration.ToString("hh\:mm\:ss"))
    printfn ""

let trainSample net sample =    
    backPropagate net sample
    
let calcCost results = 
    let errorsSquared = 
        results 
        |> List.map (fun r ->
            (0., r.output, r.sample.target) 
            |||> List.fold2 (fun s o t -> s + (t - o)**2.))
        |> List.sum
    errorsSquared / (2. * float results.Length)

let private trainCycle net getSamples checkCorrect stats testNet =
    let start = now()
    let newNet, revResults = ((net, []), getSamples()) ||> List.fold(fun (n, results) s -> 
        let r = trainSample n s
        (r.newNet, r:: results))
    let lastResult = revResults.Head
    {   net = lastResult.newNet
        cost = calcCost revResults
        sampleCount = revResults.Length
        correctCount = revResults |> List.filter checkCorrect |> List.length
        startTime = start
        duration = now() - start
        lastResult = lastResult 
        testNetResult = testNet newNet
        stats = 
        { stats with 
            duration = stats.start - now()
            cycleCount = stats.cycleCount + 1
            }}

let private createStats samplesLength = {
        start = now()
        duration = TimeSpan.Zero
        totalSampleCount = samplesLength
        cycleCount = 0  }

let trainSeries net samples checkCorrect testNet =
    trainCycle net (fun _ -> samples) checkCorrect (createStats samples.Length) testNet

let getTrainingSetProvider samples =
    let size = Array.length samples
    let getIndexes =
        let primes = getPrimes (int (sqrt (float size))) (Int32.MaxValue / size)
        fun () -> 
            let prime = primes.[rnd.Next(primes.Length)]
            [1..size] |> List.map (fun i -> (i * prime) % size)    
    fun () -> 
        getIndexes() |> List.map (fun i -> samples.[i])          

let private trainUntilWithStats net samples checkCorrect checkDone stats testNet =
    let trainingSetProvider = getTrainingSetProvider samples
    let rec runAndCheck net stats =
        let r = trainCycle net trainingSetProvider checkCorrect stats testNet
        display r
        match checkDone r with
        | true -> display r; r
        | false -> runAndCheck r.lastResult.newNet r.stats
    runAndCheck net stats

let trainUntil net samples checkCorrect checkDone =
     trainUntilWithStats net samples checkCorrect checkDone (createStats samples.Length)

let rec trainIncrementally net samples checkCorrect checkDone start testNet =
    let stats = createStats (Array.length samples)
    let samplesLength = Array.length samples
    match start >= samplesLength with
    | false ->
        let r = trainUntilWithStats net samples.[1..start] checkCorrect checkDone stats testNet
        let increment = max 1 (start / 13)
        trainIncrementally  r.net samples checkCorrect checkDone (start + increment) testNet
    | true ->
        trainUntilWithStats net samples checkCorrect checkDone stats
