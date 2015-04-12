open System

//type connection = float
//type neuron = connection list
//type layer = neuron list
//type net = layer list
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
    let inLayer = []
    inLayer ::
        [for _ in 1..hiddenCount -> createLayer inputCount connectionCount]
        @ [outLayer]

let evalNeuron values neuron=
    (0.0, neuron, values) |||> List.fold2 
        (fun sum connection value -> sum + connection * value)

let evalLayer values layer =
    bias::(layer |> List.map (evalNeuron values))

let evalNet net inputs =
    let values = bias::inputs
    let layers = List.tail net
    (values, layers) ||> List.fold evalLayer
                    

[<EntryPoint>]
let main argv = 

    let net = createNet 2 0

    let inputs = [2.0; 2.0]

    let max = 
        [1..1000000] |> List.map (fun _ ->
            (evalNet (createNet 2 0) inputs).[1]
        ) |> List.max
    Console.WriteLine max
    Console.ReadKey() |> ignore
    0 
