module ANN

let bias = 1.
let learningConstant = 0.9

// Creating
let rnd = System.Random()
let createNet layerSizes =    
    let createNeuron inputLayerSize = 
        [for i in 1 .. 1 + inputLayerSize -> rnd.NextDouble() * 2. - 1.]
    let createLayer inputLayer neuronCount  = 
        [for i in 1 .. neuronCount -> createNeuron (List.length inputLayer)]
    (([], layerSizes) ||> List.scan createLayer).Tail.Tail

// FeedForward
let evaluateLayers net inputs =
    (inputs, net) ||> List.scan (fun inputs layer ->
        layer |> List.map (fun neuron ->
            (0., neuron, bias::inputs)
            |||> List.fold2 (fun sum weight input-> sum + weight * input)
            |> (fun sum -> 1. / (1. + exp -sum))))

let feedForward net inputs =
    evaluateLayers net inputs |> List.reduce (fun _ l -> l)

// BackPropagation helper functions...
let rec private bodyAndTail list =
    let r = (List.rev list)
    (List.rev r.Tail, r.Head)
   
let rec private mTranspose  = function
    | (_::_)::_ as M -> List.map List.head M :: mTranspose (List.map List.tail M)
    | _ -> []

let private mIdentity size =
    [for x in [1..size] -> [for y in [1..size] -> if x = y then 1. else 0.]]

// BackPropagation types
type Sample = {input : float list; target : float list}

type BackPropagationResult = {
    sample : Sample
    output : float list
    newNet : float list list list
}

type private BpData = {
    newNet : float list list list 
    values : float list
    weightsBetween : float list list
    outputLayerErrors : float list
}

// BackPropagation
let backPropagate net sample =
    
    let buildNewLayer layerInput oldLayer bpData =         
        let neuronErrors = 
            (bpData.weightsBetween, bpData.values) 
            ||> List.map2 (fun weightsBetween value -> 
                ((0., weightsBetween, bpData.outputLayerErrors) 
                    |||> List.fold2 (fun s w e -> s + w*e)) * value * (1. - value))
        let newLayer =
            (oldLayer, neuronErrors) 
                ||> List.map2 (fun oldNeuron neuronError ->
                (bias::layerInput, oldNeuron) ||> List.map2 (fun input oldWeight -> 
                    oldWeight + learningConstant * neuronError * input))
        let newBpState = {
             newNet = newLayer::(bpData.newNet)
             values = layerInput 
             weightsBetween = (mTranspose oldLayer).Tail
             outputLayerErrors = neuronErrors}
        newBpState

    let allValues = evaluateLayers net sample.input
    let inputsByLayer, output = bodyAndTail allValues

    let initialBpState = {
        newNet = []
        values = output
        weightsBetween = (mIdentity output.Length)
        outputLayerErrors = (sample.target, output) ||> List.map2 (-) }
  
    let finalBpState = 
        (inputsByLayer, net, initialBpState) 
        |||> List.foldBack2 buildNewLayer  
    {
        sample = sample
        output = output
        newNet = finalBpState.newNet
    }
