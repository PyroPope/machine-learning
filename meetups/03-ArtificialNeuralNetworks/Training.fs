module Training
open System
open ANN
open Primes

type SessionStats =
    {   start : DateTime
        duration : TimeSpan
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
    printfn ""

let trainSample net learnRate sample =    
    backPropagate net learnRate sample
    
let calcCost results = 
    let errorsSquared = 
        results 
        |> List.map (fun r ->
            (0., r.output, r.sample.target) 
            |||> List.fold2 (fun s o t -> s + (t - o)**2.))
        |> List.sum
    errorsSquared / (2. * float results.Length)

let private trainCycle net learnRate samples checkCorrect stats testNet =
    let start = now()
    let newNet, revResults = ((net, []), samples) ||> List.fold(fun (n, results) s -> 
        let r = trainSample n learnRate s
        (r.newNet, r:: results))
    let sampleCount = samples.Length
    let lastResult = revResults.Head
    {   net = lastResult.newNet
        cost = calcCost revResults
        sampleCount = sampleCount
        correctCount = revResults |> List.filter checkCorrect |> List.length
        startTime = start
        duration = now() - start
        lastResult = lastResult 
        testNetResult = testNet newNet sampleCount
        stats = 
        { stats with 
            duration = stats.start - now()
            cycleCount = stats.cycleCount + 1
            }}

let private createStats samplesLength = {
        start = now()
        duration = TimeSpan.Zero
        cycleCount = 0  }

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
        | true -> display r; r
        | false -> runAndCheck r.lastResult.newNet r.stats
    runAndCheck net stats

let trainUntil net learnRate samples checkCorrect checkDone =
     trainUntilWithStats net learnRate samples checkCorrect checkDone (createStats samples.Length)

let rec trainIncrementally net learnRate samples checkCorrect checkDone start testNet onIncrement =
    let stats = createStats (Array.length samples)
    let samplesLength = Array.length samples
    match start >= samplesLength with
    | false ->
        let r = trainUntilWithStats net learnRate samples.[1..start] checkCorrect checkDone stats testNet
        let newStart = start + max 1 (start / 13)
        onIncrement start newStart r.net
        trainIncrementally  r.net learnRate samples checkCorrect checkDone newStart testNet onIncrement
    | true ->
        trainUntilWithStats net learnRate samples checkCorrect checkDone stats
