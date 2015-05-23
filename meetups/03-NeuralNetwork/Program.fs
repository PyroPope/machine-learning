open System
open System.IO
open System.Text.RegularExpressions

open XOr
open Mnist

[<EntryPoint>]
let main argv =
    printfn "Hello"
    
//    CompareTheSampleDotCom.compare()
//    xor()
    
//    let session = argv.[0]
//    let learnrate = float argv.[1]
//    let trainsize = int argv.[2]   
//    mnist  session learnrate trainsize

    MnistRead.readDigits()

    printfn "Done."
    Console.ReadKey() |> ignore
    0