module ANN

let bias = 1.

type Activation = {
    learnRate : float
    activate : float -> float
    derivative : float -> float 
    minValue : float }

let sigmoidActivation = {
    learnRate = 0.1
    activate = (fun x -> 1. / (1. + exp -x))
    derivative = (fun x -> x * (1. - x))
    minValue = 0. }

let tanhActivation = {
    learnRate = 0.1
    activate = tanh
    derivative = (fun x -> 1. - (x ** 2.))
    minValue = -1. }

// Creating
let rnd = System.Random()
let createNet layerSizes =    
    let createNeuron inputLayerSize = 
        [for i in 1 .. 1 + inputLayerSize -> rnd.NextDouble() * 2. - 1.]
    let createLayer inputLayer neuronCount  = 
        [for i in 1 .. neuronCount -> createNeuron (List.length inputLayer)]
    (([], layerSizes) ||> List.scan createLayer).Tail.Tail

// FeedForward
let evaluateLayers net activation inputs =
    (inputs, net) ||> List.scan (fun inputs layer ->
        layer |> List.map (fun neuron ->
            (0., neuron, bias::inputs)
            |||> List.fold2 (fun sum weight input-> sum + weight * input)
            |> activation.activate))

let feedForward net activation inputs =
    evaluateLayers net activation inputs |> List.reduce (fun _ l -> l)

// BackPropagation helper functions...
let rec private bodyAndTail list =
    let r = (List.rev list)
    (List.rev r.Tail, r.Head)
   
//let rec mTranspose  = function
//    | (_::_)::_ as M -> List.map List.head M :: mTranspose (List.map List.tail M)
//    | _ -> []

let rec t2 m = 
    seq{
        match m with
        | (_::_)::_ -> 
            yield List.map List.head m
            yield! t2 (List.map List.tail m)
        | _ -> ()}

let private mIdentity size =
    [for x in [1..size] -> [for y in [1..size] -> if x = y then 1. else 0.]]

// BackPropagation types
type Sample = {
    input : float list  
    target : float list }

type BackPropagationResult = {
    sample : Sample
    output : float list
    newNet : float list list list }

type private BpData = {
    newNet : float list list list 
    values : float list
    weightsBetween : seq<float list>
    outputLayerErrors : seq<float> }

// BackPropagation 
let backPropagate net activation sample =
    
    let buildNewLayer layerInput oldLayer bpData =         
        let neuronErrors = 
            (bpData.weightsBetween, bpData.values) 
            ||> Seq.map2 (fun weightsBetween value -> 
                ((0., Seq.zip weightsBetween bpData.outputLayerErrors) 
                    ||> Seq.fold (fun s (w, e) -> s + w*e)) * (activation.derivative value))          
            |> List.ofSeq
        let newLayer =
            (oldLayer, neuronErrors) 
                ||> Seq.map2 (fun oldNeuron neuronError ->
                (bias::layerInput, oldNeuron) ||> List.map2 (fun input oldWeight -> 
                    oldWeight + activation.learnRate * neuronError * input))
                |> Seq.toList
        let newBpState = {
             newNet = newLayer::(bpData.newNet)
             values = layerInput 
             weightsBetween = Seq.skip 1 (t2 oldLayer)
             outputLayerErrors = neuronErrors}
        newBpState

    let allValues = evaluateLayers net activation sample.input
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
        newNet = finalBpState.newNet }
