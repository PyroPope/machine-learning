open System

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    Console.ReadKey() |> ignore
    0 
