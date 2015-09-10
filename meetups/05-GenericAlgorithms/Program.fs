open System

let ga spawn popSize select breed crossover terminate  = 
    let rec generate population generation =
        if generation % 100 = 0 then printfn "starting generation %i" generation
        let newPop =
            population
                |> select
                |> breed popSize crossover 
        /// F# how to do this with Seq without re-evaluating sequence
        if terminate (Array.head newPop) then
            newPop
        else
            generate newPop (generation + 1)
    let population = Array.init popSize (fun _ -> spawn())
    generate population 0

let spawn fitness create =     
    let noob = create()
    (noob, fitness noob)

let select population =
    population
        |> Array.sortBy (snd)
        |> Array.skip (population.Length / 2)

let breed targetSize crossover  population = [| 
    yield! population
    let popSize = Array.length population
    if popSize < 2 then
        yield! Array.empty
    else
        let random = new Random()
        let pickPoint () = Math.Pow(random.NextDouble(), 2.0)
        let pickFirst () = int (pickPoint () * float popSize)
        let pickSecond first =
            let pick = int (pickPoint () * float (popSize - 1))
            if pick < first then pick else pick + 1
        for _ in [1..(targetSize - popSize)] do
            let first = pickFirst ()
            let second = pickSecond first
            yield crossover population.[first] population.[second] |]
    

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

    //let select p = printfn "select";  p
    //let breed cross = id
    let crossover first second = first
    let terminate item = false

    let x =  ga (fun _ -> spawn fitness createString) popSize select breed crossover terminate 

    printfn "%A" argv
    0 
