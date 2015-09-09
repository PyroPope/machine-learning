open System

let ga select cross mutate terminate population : 'peep[] = 
    let rec generate population generation =
        if generation % 100 = 0 then printfn "starting generation %i" generation
        let newPop =
            population
                |> select
                |> cross mutate
        if terminate (Array.head newPop) then
            newPop
        else
            generate newPop (generation + 1)
    generate population 0


let spawn fitness create =     
    let noob = create()
    (noob, fitness noob)


let select population =
    population
        |> Array.sortBy (snd)
        |> Array.skip (population.Length / 2)


//let breed crossover population targetSize =
//    
    

///////////////



let random = new Random()

let getRandomSelection count (items:'a[])  =
    Array.init count (fun _ -> items.[random.Next(items.Length)])


let distance target peep =
    peep 
        |> Array.zip target
        |> Array.map (fun (a, b) -> if a = b then 1 else 0)
        |> Array.sum
        |> (fun sum -> float sum / float target.Length)

    
[<EntryPoint>]
let main argv = 

    let text = "To be or not to be."
    let fitness = distance (text.ToCharArray())  
    
    let chars =  [|32..126|] |> Array.map char
    let createString() = getRandomSelection text.Length chars

    
    let popSize = 64
    let pop = 
        [| for i in [1..popSize] -> spawn fitness createString |]

    let select = id
    let cross mutate = id
    let mutate = id
    let terminate item = false

    let x = ga select cross mutate terminate pop

    printfn "%A" argv
    0 
