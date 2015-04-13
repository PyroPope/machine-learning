open System

let bias = 1.0

let rnd = System.Random()
let randomWeight() =  rnd.NextDouble() * 2.0 - 1.0

let createNeuron connectionCount =
    [for i in 1 .. connectionCount -> randomWeight()]

let createLayer neuronCount connectionCount =
    [for i in 1 .. neuronCount -> createNeuron connectionCount]
    
let createNet inputCount hiddenCount = 
    let connectionCount = inputCount + 1
    let outLayer = createLayer 1 connectionCount
    [for _ in 1..hiddenCount -> createLayer inputCount connectionCount]
        @ [outLayer]

let sigmoid x = 1.0 / (1.0 + exp -x)

let evalNeuron layerOutputs neuron =
    let sum = ((0.0, neuron, layerOutputs) |||> List.fold2 
        (fun sum connection output -> sum + connection * output))
    sigmoid sum

let evalLayer (netOutputs : float list list) layer =
    let ouputs = (layer |> List.map (evalNeuron netOutputs.Head))
    (bias::ouputs)::netOutputs

let evalNet net inputs =
    let netOutputs = ([bias::inputs], net) ||> List.fold evalLayer
    (netOutputs.Head.[1], netOutputs)
                    

[<EntryPoint>]
let main argv = 

    let net = createNet 2 4

    let inputs = [1.0; 1.0]

    let max = 
        [1..100000] |> List.map (fun _ ->
            fst (evalNet (createNet 2 4) inputs)
        ) |> List.max
    Console.WriteLine max
    Console.ReadKey() |> ignore
    0 
