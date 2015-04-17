open System

let bias = 1.0
let learningConstant = 0.5

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
    sigmoid sum

let evalLayer inputs layer =
    (layer |> List.map (evalNeuron (bias::inputs)))

let evalNet net netInputs =
    net |> List.scan evalLayer netInputs

// Training
let bodyAndTail list =
    let r = List.rev list
    (List.rev r.Tail, r.Head)

let rec transpose  = function
    | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []

let zip4 a b c d =
    (List.zip a b, c, d) |||> List.map3 (fun (a, b) c d -> (a, b, c, d))

//let lastItem list =
//    list |> List.reduce (fun _ i -> i)

//let pairUp list = 
//    (([],(List.head list)), list.Tail) ||> List.scan (fun lastPair nextItem -> (snd lastPair, nextItem))

let trainNet net args answers =
    let allValues = evalNet net args
    let allInputs, netOutputs = bodyAndTail allValues

    let newWeightDelta neuronDelta input = learningConstant * neuronDelta * input
    let newWeight neuronDelta input oldWeight  = oldWeight + (newWeightDelta neuronDelta input)
    let neuronDelta output outputWeights outputDeltas =
        let Z = (outputWeights, outputDeltas) ||> List.map2 (*)  |> List.sum
        output * (1.0 - output) * Z        
    let newNeuronAndDelta inputs outputDeltas outputWeights  output oldNeuron = 
        let neuronDelta = neuronDelta output outputWeights outputDeltas
        let newNeuron = (bias::inputs, oldNeuron) ||> List.map2 (newWeight output)
        (newNeuron, neuronDelta)
    let newLayerWithDeltas bpData inputs  oldLayer = 
        let _, outputs, outputWeights, outputDeltas  = bpData
        ((List.tail outputWeights), outputs, oldLayer) |||> List.map3 (newNeuronAndDelta inputs outputDeltas)
    let bpData inputs oldLayer lastBpData =
        let newNet, _, _, _ = lastBpData
        let newLayer, deltas = newLayerWithDeltas lastBpData inputs oldLayer |> List.unzip
        printfn "bpData"; (newLayer::newNet, inputs, transpose oldLayer, deltas)
    let newNet = 
        let netWeights = [1.0]::[for _ in netOutputs -> [1.0]]
        let netDeltas = List.map2 (fun answer output -> answer - output) answers netOutputs 
        let initialState = ([], netOutputs, netWeights, netDeltas)
        (allInputs, net, initialState) |||> List.foldBack2 bpData 
    ()
  


//    // arrange data
//    let allValues = evalNet net inputs
//    let allInputs, netOutput = bodyAndTail allValues
//    let hiddenLayers, outputLayer = bodyAndTail net 
//    
//    // get new output layer
//    let outputNeuronDelta output answer = output * (1.0 - output) * (answer - output)
//    let netOutputDeltas = (netOutput, answers) ||> List.map2 outputNeuronDelta
//    let netDelta =  netOutputDeltas |> List.sum
//    let newWeight  delta output oldWeight= oldWeight + (learningConstant * output * delta)
//    let newOutputNeuron  delta output oldWeights = oldWeights |> List.map (newWeight delta output)
//    let newOutputLayer = (netOutputDeltas, netOutput, outputLayer) |||> List.map3 newOutputNeuron
//
//    // arrange more
//    let hiddenOutputs = allInputs.Tail
//    let hiddenInputs = fst (bodyAndTail allInputs)
//    let hiddenWeightsOut = (net |> List.map transpose).Tail 
//    let hiddenLayerInfo = zip4 hiddenInputs hiddenLayers hiddenOutputs hiddenWeightsOut
//
//    // build new hidden layers
//    let newWeight oldWeight delta = oldWeight + delta
//    let weightDelta = 


//        let delta = sum * input * (1.0 - input)         
//        newWeight delta output oldWeight
//
//    let newHiddenNeuron weightsOut =
//        let sum = List.sum weightsOut * netDelta



// sum = (sum of outgoing weights) * netDelta
// foreach incoming weight
    //delta = output * (1 - output) * sum 
    // newWeight

//
//    printfn "%A" netOutput
//    let newNet = hiddenLayers 
//    @ [newOutputLayer]
//    printfn "%A" ((evalNet newNet inputs) |> bodyAndTail |> snd)

    0    

[<EntryPoint>]
let main argv = 
    let inputs = [1.0; 1.0]
    let net = createNet 2 [4] 1
    let _ = trainNet net inputs [0.0]
    //printfn "%A" net

    Console.ReadKey() |> ignore


//let getFroms net =
//    let rec transpose  = function
//        | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
//        | _ -> []
//    List.map transpose net
//
//let train net inputs answer=
//    let out, outsR = evalNet net inputs
//    let hlOutputs = outsR.Tail.Tail
//    let dOut = out * (1.0 - out) * (answer - out)
//
//    let netR = List.rev net
//    let hlWeights = List.rev net.Tail
//    let hlWeightOut = getFroms netR
//    let outLayer, hiddenLayers = netR.Head, netR.Tail
//    let outLayerValues, layersValues = outsR.Head, outsR.Tail
//    let prevValues, layersValues = layersValues.Head, layersValues.Tail
//    let newOutLayer = outLayer |> List.map (fun n -> (( n, prevValues) ||> List.map2(fun w pv-> w + lc * (pv * dOut))))
//    
//    let newHidden =
//        (hlOutputs, hlWeightOut, hlWeights) 
//        |||> List.map3 (fun lOutputs lWeightsOut lWeights -> 
//                (lOutputs, lWeightsOut, lWeights) |||> List.map3 (fun outut weightsOut weights ->
//                    let sum = (0.0, weightsOut) ||> List.fold (fun sum weightOut -> sum + weightOut * dOut)
//                   
//                    0
//                )
//            )
//
//
//    printfn "out %A" out
//    printfn "%A" outLayer
//    printfn "%A" prevValues
//    printfn "%A" newOutLayer
//    printfn "%A" (fst (evalNet newNet inputs))
//    0


    0 
