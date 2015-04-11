open System

let getNetwork inputCount hiddenCount =
    fun (inputs : float array) -> Array.sum inputs


[<EntryPoint>]
let main argv = 
    let net = getNetwork 0 0;
    let out = net [|1.0; 2.0; 3.0; 4.0|]

    Console.WriteLine out
    Console.ReadKey() |> ignore
    0 
