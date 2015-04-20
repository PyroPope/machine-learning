open System

let bias = 1.0
let learningConstant = 0.9

// Creating
let rnd = System.Random()
let randomWeight() =  rnd.NextDouble() * 2.0 - 1.0

let createNeuron inputCount =
    [for i in 1 .. inputCount -> randomWeight()]

let createLayer neuronCount inputCount =
    [for i in 1 .. neuronCount -> createNeuron (inputCount + 1)]
    
let createNet netInputCount hiddenLayerSizes outputLayerSize = 
    let neuronLayerSizes = hiddenLayerSizes @ [outputLayerSize]
    let inputCounts = netInputCount :: hiddenLayerSizes
    (neuronLayerSizes, inputCounts) ||> List.map2 createLayer

// Evaluating
let sigmoid x = 1.0 / (1.0 + exp -x)

let evalNeuron inputs neuron =
    let aggregate sum a b = sum + a * b
    let sum = ((0.0, neuron, inputs) |||> List.fold2 aggregate)
    let result = sigmoid sum
    result

let evalLayer inputs layer =
    (layer |> List.map (evalNeuron (bias::inputs)))

let evalNet net netInputs =
    net |> List.scan evalLayer netInputs

let evalOutputs net netInputs =
    evalNet net netInputs |> List.reduce (fun _ l -> l)

// Training
let bodyAndTail list =
    let r = List.rev list
    (List.rev r.Tail, r.Head)

let rec transpose  = function
    | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []

let trainNet net args answers =
    let allValues = evalNet net args
    let allInputs, netOutputs = bodyAndTail allValues

    let calcNewWeight neuronError inputWeight oldWeight  = 
        oldWeight + learningConstant * neuronError * inputWeight
    let calcNeuronError output outputWeights outputErrors =
        ((outputWeights, outputErrors) ||> List.map2 (*)  |> List.sum) 
        * output * (1.0 - output) 
    let newNeuronAndError inputs outputErrors outputWeights  output oldNeuron = 
        let neuronError = calcNeuronError output outputWeights outputErrors
        let newNeuron = (bias::inputs, oldNeuron) ||> List.map2 (calcNewWeight neuronError)
        (newNeuron, neuronError)
    let newLayerAndErrors bpData inputs  oldLayer = 
        let _, outputs, outputWeights, outputErrors  = bpData
        ((List.tail outputWeights), outputs, oldLayer) |||> List.map3 (newNeuronAndError inputs outputErrors)
    let getBpData inputs oldLayer lastBpData =
        let newNet, _, _, _ = lastBpData
        let newLayer, errors = newLayerAndErrors lastBpData inputs oldLayer |> List.unzip
        (newLayer::newNet, inputs, transpose oldLayer, errors)
    let newNet, _, _, _ = 
        let netWeights = [1.0]::[for _ in netOutputs -> [1.0]]
        let netDeltas = List.map2 (fun answer output -> answer - output) answers netOutputs 
        let initialState = ([], netOutputs, netWeights, netDeltas)
        (allInputs, net, initialState) |||> List.foldBack2 getBpData 
    newNet
  
let calcCost samples (evalOutput) =
    let sumOfSquares, count = ((0.0, 0), samples) ||> List.fold (fun (sum, count) sample ->
        let inputs, answer = sample;
        let outputs = evalOutput inputs
        let sos = (answer, outputs) ||> List.map2 (fun a o -> (a-o)**2.0) |> List.sum
        (sum + sos, count + 1)
    )
    sumOfSquares / (2.0 * float count)

let compareWithAppBExample() =
    let inputs = [1.0; 0.0; 1.0]
    let answer = [1.0]
    let net = [[[-0.4; 0.2; 0.4; -0.5; ]; [0.2; -0.3; 0.1; 0.2] ];
                    [[0.1; -0.3; -0.2]]]
    let expectedValues = [[1.0; 0.0; 1.0]; [0.3318122278; 0.5249791875]; [0.4738888988]]
    let expectedNet = [[[-0.4078521058; 0.1921478942; 0.4; -0.5078521058];
                        [0.1941121234; -0.3058878766; 0.1; 0.1941121234]];
                        [[0.2180521704; -0.2608288463; -0.1380250675]]]   
    let values = evalNet net inputs
    let newNet = trainNet net inputs answer
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

let goXor() = 
    let net = createNet 2 [2] 1
    let samples = getSamples xorCases 100000
    printfn "cost before: %f" (calcCost samples (evalOutputs net) )
    xorCases |> Array.iter (fun (inputs, answer) -> printfn "%f" (evalOutputs net inputs).[0])
    let finalNet = (net, samples) ||> List.fold (fun net (input, answers) -> (trainNet net input answers))
    printfn "cost after:  %f" (calcCost samples (evalOutputs finalNet) )
    xorCases |> Array.iter (fun (inputs, answer) -> printfn "%f" (evalOutputs finalNet inputs).[0])
  

[<EntryPoint>]
let main argv =
    //compareWithAppBExample()
    goXor()
    
    Console.ReadKey() |> ignore
    0

