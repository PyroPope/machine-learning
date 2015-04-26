module Training
open System
open ANN

type SessionStats =
    {   start : DateTime
        duration : TimeSpan
        sampleCount : int
        cycleCount : int
    }

type CycleResult =
    {   net : float list list list
        cost : float
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

let private trainCycle net samples checkCorrect stats =
    let start = now()
    let results = samples |> List.map (trainSample net)
    let correctCount = results |> List.filter checkCorrect |> List.length
    let cost = calcCost results
    let lastResult = results |> List.reduce (fun _ l -> l)
    {   net = lastResult.newNet
        cost = cost
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
    printfn "  cost:                   %.15f" r.cost
    let sampleCount = r.stats.sampleCount
    let correctPercent = 100.0 * float r.correctCount / float sampleCount
    printfn "  correct:                %.2f%% (%d of %d)" correctPercent r.correctCount sampleCount
    printfn "  samples:       %d" sampleCount
    printfn "  cycle time:    %s" (r.duration.ToString("hh\:mm\:ss"))
    printfn "  run time:      %s" (r.stats.duration.ToString("hh\:mm\:ss"))

//             test set:      565
//  cycle time:    00:00:22
//  run time:      00:20:18

//starting training cycle 57,  done.
       
let trainSeries net samples checkCorrect =
    display (trainCycle net samples checkCorrect {
        start = now()
        duration = TimeSpan.Zero
        sampleCount = samples.Length
        cycleCount = 0  })

 
     
