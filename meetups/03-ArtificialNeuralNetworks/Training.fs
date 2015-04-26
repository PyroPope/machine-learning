module Training

open ANN

type TrainingResult =
    {   net : float list list list
        cost : float
        correctCount : int
        bpResults : BackPropagationResult list }

let trainSample net sample =
    backPropagate net sample

let trainSeries net samples checkCorrect =
    let results = samples |> List.map (trainSample net)
    let errorsSquared = 
        results 
        |> List.map (fun r ->
            (0., r.output, r.sample.target) 
            |||> List.fold2 (fun s o t -> s + (t - o)**2.))
        |> List.sum
    let cost = errorsSquared / (2. * float results.Length)
    let correctCount = results |> List.filter checkCorrect |> List.length
    let lastResult = results |> List.reduce (fun _ l -> l)
    {   net = lastResult.newNet
        cost = cost
        correctCount = correctCount
        bpResults = results }
 
     
