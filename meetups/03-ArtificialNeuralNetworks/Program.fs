open System
open System.IO
open System.Text.RegularExpressions

open ANN
open Persistence



// Measure
let calcCost samples (evalOutput) =
    let sumOfSquares, count = ((0.0, 0), samples) ||> List.fold (fun (sum, count) sample ->
        let inputs, answer = sample;
        let outputs = evalOutput inputs
        let sos = (0.0, answer, outputs) |||> List.fold2 (fun s a o -> s + (a-o)**2.0) 
        (sum + sos, count + 1)
    )
    sumOfSquares / float count

// Running...

// Compare to numeric example
let compareTheSampleDotCom() =
    let inputs = [1.0; 0.0; 1.0]
    let answer = [1.0]
    let net = [[[-0.4; 0.2; 0.4; -0.5; ]; [0.2; -0.3; 0.1; 0.2] ];
                    [[0.1; -0.3; -0.2]]]
    let expectedValues = [[1.0; 0.0; 1.0]; [0.3318122278; 0.5249791875]; [0.4738888988]]
    let expectedNet = [[[-0.4078521058; 0.1921478942; 0.4; -0.5078521058];
                        [0.1941121234; -0.3058878766; 0.1; 0.1941121234]];
                        [[0.2180521704; -0.2608288463; -0.1380250675]]]   
    let values = evaluateLayers net inputs
    let newNet = backPropagate net inputs answer
    printfn "newNet   %A" newNet
    printfn "expected %A" expectedNet

let getSamples cases count = 
    let size = Array.length cases
    [for _ in [1..count] -> cases.[rnd.Next(size)]]

let xorCases = [|
    ([0.0; 0.0], [0.0]);
    ([0.0; 1.0], [1.0]);
    ([1.0; 0.0], [1.0]);
    ([1.0; 1.0], [0.0]);
|]

// Train XOr
let goXor() = 
    let net = createNet [2; 4; 1]
    let samples = getSamples xorCases 10000
    printfn "cost before: %f" (calcCost samples (feedForward net) )
    xorCases |> Array.iter (fun (inputs, answer) -> printfn "%f" (feedForward net inputs).[0])
    let finalNet = (net, samples) ||> List.fold (fun net (input, answers) -> (backPropagate net input answers))
    printfn "cost after:  %f" (calcCost samples (feedForward finalNet) )
    xorCases |> Array.iter (fun (inputs, answer) -> printfn "%f" (feedForward finalNet inputs).[0])

let maxIndex list =
    list
    |> List.mapi(fun i l -> i, l)
    |> List.maxBy snd
    |> fst

// Train digit recognizer
let goDigits sampleSize = 
    // 80; 512; 3280; 21000 ?
    // 5; 40; 324; 2609; 21000;
    let now() = DateTime.UtcNow
    let start = now()
    printfn "Loading data and calculating initial values ..."
 
    let buildAnswerList count index  =
        [for i in 0.0..(count - 1.0) -> if i = index then 1.0 else 0.0]
    let getValues fileName count= 
        let trainFile = __SOURCE_DIRECTORY__ + @"\digits\" + fileName;
        (File.ReadAllLines trainFile).[1..count] 
        |> Array.map (fun line -> line.Split(',')) |> Array.toList
        |> List.map (fun fields -> fields |> Array.toList |> List.map (float))        
    let sampleCases = 
        getValues "training-20000.csv" sampleSize
        |> List.map (fun vals -> (vals.Tail, buildAnswerList 10.0 vals.Head))
    printfn "Sample count: %d" sampleCases.Length
    let testValues = 
        getValues "training-1000.csv" 1000
        |> List.map (fun vals -> (vals.Tail, buildAnswerList 10.0 vals.Head))
    
    let stateFile = sprintf "digits" 
    let net = 
        match tryLoad stateFile with 
        // http://yann.lecun.com/exdb/mnist/  -> 500-100 
        // (wiki MNIST database) -> 784 [2500; 2000; 1500; 1000; 500] 10 
        | None -> printfn "creating new net"; createNet [784; 500; 100; 10]
        | Some(net) -> printfn "loading existing net from \"%s\"" stateFile; net

    let countCorrectTraining net = 
        sampleCases 
        |> List.filter (fun (inputs, answer) -> maxIndex (feedForward net inputs) = maxIndex answer) 
        |> List.length
    
    let countCorrectTesting net = 
        testValues 
        |> List.filter (fun (inputs, answer) -> maxIndex (feedForward net inputs) = maxIndex answer) 
        |> List.length
    
    let cases = Array.ofList sampleCases
    let caseCount = cases.Length
    let getRandomSamples() = 
        [for _ in [1..caseCount] -> cases.[rnd.Next(caseCount)]]  

    let getPrimes greaterThan =
        __SOURCE_DIRECTORY__  + "\TheFirst10,000Primes.txt"
        |> File.ReadAllLines
        |> Array.filter  (fun l ->  Regex.IsMatch(l, @"^\s*\d"))
        |> Array.collect  (fun l -> Regex.Split(l, @"\s+", RegexOptions.Singleline))
        |> Array.filter (fun t -> not (String.IsNullOrEmpty t))
        |> Array.map (fun t -> int t)
        |> Array.filter (fun i -> i > greaterThan && i < 100000)  // < 100,000 to prevent int overflow
    let primes = getPrimes (sampleSize |> float |> sqrt |> int)
    let getRandomWalk() =
        let prime = primes.[rnd.Next(primes.Length)]
        [0..sampleSize]
        |> List.map (fun i -> (i * prime) % sampleSize)
        |> List.map (fun index -> sampleCases.[index])
      
    printfn "initial cost:  %f" (calcCost sampleCases (feedForward net))
    printfn "correct count: %d" (countCorrectTraining net)
    printfn "testing count: %d" (countCorrectTesting net)
    printfn ""

    let train net samples = (net, samples) ||> List.fold (fun net (input, answers) -> (backPropagate net input answers))
    let finalNet = 
        (net, [1..100000]) 
        ||> List.fold (fun net cycleNumber -> 
            let cycleStart = now()
            printf "starting training cycle %d, " cycleNumber

//            let samples = sampleCases
//            let samples = getRandomSamples()
            let samples = getRandomWalk()

            let newNet = train net samples
            printf " d"
            save stateFile newNet
            printfn "one."
            
            let correctCount = countCorrectTraining newNet
            let correctPercent = 100.0 * float correctCount / float sampleSize
            printfn "  cost:                   %.15f" (calcCost sampleCases (feedForward newNet))
            printfn "  correct:                %.2f%% (%d of %d)" correctPercent correctCount sampleSize
            printfn "  test set:      %d" (countCorrectTesting newNet)
            printfn "  cycle time:    %s" ((now() - cycleStart).ToString("hh\:mm\:ss"))
            printfn "  run time:      %s" ((now() - start).ToString("hh\:mm\:ss"))
            printfn ""
            newNet )
    ()


[<EntryPoint>]
let main argv =
    printfn "hello."
    //compareTheSampleDotCom()
    //goXor()
    //let sampleSize = if argv.Length > 0 then int argv.[0] else 324
    goDigits 85

    printfn "done."
    Console.ReadKey() |> ignore
    0