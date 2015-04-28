open System
open System.IO
open System.Text.RegularExpressions

open XOr
open Mnist

[<EntryPoint>]
let main argv =
    printfn "Hello"
    
    //CompareTheSampleDotCom.compare()
    //xor()
    
    let trainSize = if argv.Length > 0 then int argv.[0] else 40
    mnist trainSize
    
    printfn "Done."
    Console.ReadKey() |> ignore
    0