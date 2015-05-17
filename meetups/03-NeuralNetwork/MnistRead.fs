module MnistRead

open System.IO

open ANN
open MnistData
open Persistence

let maxIndex list =
    list
    |> List.mapi(fun i l -> i, l)
    |> List.maxBy snd
    |> fst

let loadNet =
    let fileName = "GuessNet"
    match tryLoad(fileName) with
    | None -> failwith "Couldnt load net from file %s" fileName
    | Some(net) -> net

let readDigits() =
    printf "Loading net... "
    let net = loadNet
    printfn "done"
    printf "Loading data... "
    let data = mnistTesting 
    printfn "done"
    printf "Reading... "
    let guesses = 
        data 
        |> Seq.map (feedForward net sigmoidActivation)
        |> Seq.map maxIndex
    printfn "done"
    printf "Writing... "
    use stream  = File.OpenWrite (Persistence.stateDir + "\guesses.txt")
    use writer = new StreamWriter(stream)
    guesses |> Seq.iter writer.WriteLine
    printfn "done"


