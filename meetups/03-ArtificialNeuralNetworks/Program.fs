open System

let bias = 1.0
//let learningConstant = 0.5
//
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

let sigmoid x = 1.0 / (1.0 + exp -x)

let evalNeuron inputs neuron =
    let sum = ((0.0, neuron, inputs) |||> List.fold2 
        (fun sum connection output -> sum + connection * output))
    sigmoid sum

let evalLayer inputs layer =
    (layer |> List.map (evalNeuron (bias::inputs)))

let evalNet net netInputs =
    let outPutsByLayer = net |> List.scan evalLayer netInputs 
    let netOutputs = outPutsByLayer |> List.reduce (fun _ i -> i)
    netOutputs



[<EntryPoint>]
let main argv = 
    let inputs = [1.1; 1.2]
    let net = createNet 2 [4] 1
    printfn "%A" (evalNet net inputs)

    Console.ReadKey() |> ignore


//    let netOutputs = ([bias::inputs], net) ||> List.fold evalLayer
//    
//    0
//    outputs
//    (netOutputs.Head.[1], netOutputs)
//            
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
