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
        stats : SessionStats}

let now() = DateTime.UtcNow

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

let private trainCycle net getSamples checkCorrect stats =
    let start = now()
    let _, revResults = ((net, []), getSamples()) ||> List.fold(fun (n, results) s -> 
        let r = trainSample n s
        (r.newNet, r:: results))
    let correctCount = revResults |> List.filter checkCorrect |> List.length
    let cost = calcCost revResults
    let lastResult = revResults.Head
    {   net = lastResult.newNet
        cost = cost
        sampleCount = revResults.Length
        correctCount = correctCount
        startTime = start
        duration = now() - start
        lastResult = lastResult 
        stats = 
        { stats with 
            duration = stats.start - now()
            cycleCount = stats.cycleCount + 1
            }}

let display r =
    printfn "Completed cycle: %d" r.stats.cycleCount
    printfn "  cost:                   %.15f" r.cost
    let sampleCount = r.sampleCount
    let correctPercent = 100.0 * float r.correctCount / float sampleCount
    printfn "  correct:                %.2f%% (%d of %d)" correctPercent r.correctCount sampleCount
    printfn "  samples:       %d" sampleCount
    printfn "  cycle time:    %s" (r.duration.ToString("hh\:mm\:ss"))
    printfn "  run time:      %s" (r.stats.duration.ToString("hh\:mm\:ss"))
    printfn ""
    r

let private createStats samplesLength = {
        start = now()
        duration = TimeSpan.Zero
        totalSampleCount = samplesLength
        cycleCount = 0  }

let trainSeries net samples checkCorrect =
    trainCycle net (fun _ -> samples) checkCorrect (createStats samples.Length)

let getTrainingSetProvider samples =
    let size = Array.length samples
    let getIndexes =
        let primes = getPrimes (int (sqrt (float size))) (Int32.MaxValue / size)
        fun () -> 
            let prime = primes.[rnd.Next(primes.Length)]
            [1..size] |> List.map (fun i -> (i * prime) % size)    
    fun () -> 
        getIndexes() |> List.map (fun i -> samples.[i])          

let private trainUntilWithStats net samples checkCorrect checkDone stats =
    let trainingSetProvider = getTrainingSetProvider samples
    let rec runAndCheck net stats =
        let r = trainCycle net trainingSetProvider checkCorrect stats 
        match checkDone r with
        | true -> r
        | false -> runAndCheck r.lastResult.newNet r.stats
    display( runAndCheck net stats )

let trainUntil net samples checkCorrect checkDone =
     trainUntilWithStats net samples checkCorrect checkDone (createStats samples.Length)


let rec trainIncrementally net samples checkCorrect checkDone start =
    let stats = createStats (Array.length samples)
    let samplesLength = Array.length samples
    match start >= samplesLength with
    | false ->
        let r = trainUntilWithStats net samples.[1..start] checkCorrect checkDone stats
        trainIncrementally  r.net samples checkCorrect checkDone (start + 1)
    | true ->
        trainUntilWithStats net samples checkCorrect checkDone stats


 
     
