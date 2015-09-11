open System

let sw = new System.Diagnostics.Stopwatch()
sw.Start()

let ga create popSize fitness select breed crossover terminate  = 
    let rec generate population generation nextUpdateTime =        
        let newPop =
            population
                |> select
                |> breed popSize crossover 
        
        let best = Array.head population
        let bestString = new String(fst best: char[])
        let bestFit = snd best
        let display () =  printfn "%s \n\n>completed generation %i %s %f" bestString generation (sw.Elapsed.ToString(@"hh\:mm\:ss\.fff")) bestFit
        let nextUpdateTime = 
            if DateTime.UtcNow > nextUpdateTime then
                display ()
                DateTime.UtcNow.AddSeconds(1.0)
            else
                nextUpdateTime
        if terminate bestFit then
            display ()
            printfn "Done!" 
            Console.ReadKey() |> ignore
            newPop
        else
            generate newPop (generation + 1) nextUpdateTime
    let addFitness peep = (peep, fitness peep)
    let population = 
        Array.init popSize (fun _ -> addFitness (create ()))
    generate population 0 DateTime.MinValue

let select population =
    population
        |> Array.sortBy (snd)
        |> Array.skip (population.Length / 2)

let breed getMutationRate targetSize crossover population = [| 
    // will blow up of popSize < 2
    yield! population
    let popSize = Array.length population
    let random = new Random()
    let pickPoint () = Math.Pow(random.NextDouble(), 1.4)
    let pickFirst () = int (pickPoint () * float popSize)
    let pickSecond first =
        let pick = int (pickPoint () * float (popSize - 1))
        if pick < first then pick else pick + 1    
    let mutationRate = getMutationRate population
    for _ in [1..(targetSize - popSize)] do
        let first = pickFirst ()
        let second = pickSecond first
        yield crossover mutationRate population.[first] population.[second] |]
 
let getMutationRate population = 0.5

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

let crossover create fitness mutationRate first second =
    let xavier = create ()
    let firstPeeps, secondPeeps = fst first, fst second
    let mutationPerPeep = mutationRate / float (Array.length firstPeeps)
    let child =
        (firstPeeps, secondPeeps, xavier)
        |||> Array.map3  (fun f s m -> 
            if random.NextDouble() < mutationPerPeep then m
            else if random.NextDouble() < 0.5 then f else s )
        |> (fun c -> (c, fitness c))
    child

    
[<EntryPoint>]
let main argv = 

    let popSize = 3 * 4
    let text =  @"To be, or not to be--that is the question:
Whether 'tis nobler in the mind to suffer
The slings and arrows of outrageous fortune
Or to take arms against a sea of troubles
And by opposing end them. To die, to sleep--
No more--and by a sleep to say we end
The heartache, and the thousand natural shocks
That flesh is heir to. 'Tis a consummation
Devoutly to be wished. To die, to sleep--
To sleep--perchance to dream: ay, there's the rub,
For in that sleep of death what dreams may come
When we have shuffled off this mortal coil,
Must give us pause. There's the respect
That makes calamity of so long life.
For who would bear the whips and scorns of time,
Th' oppressor's wrong, the proud man's contumely
The pangs of despised love, the law's delay,
The insolence of office, and the spurns
That patient merit of th' unworthy takes,
When he himself might his quietus make
With a bare bodkin? Who would fardels bear,
To grunt and sweat under a weary life,
But that the dread of something after death,
The undiscovered country, from whose bourn
No traveller returns, puzzles the will,
And makes us rather bear those ills we have
Than fly to others that we know not of?
Thus conscience does make cowards of us all,
And thus the native hue of resolution
Is sicklied o'er with the pale cast of thought,
And enterprise of great pitch and moment
With this regard their currents turn awry
And lose the name of action. -- Soft you now,
The fair Ophelia! -- Nymph, in thy orisons
Be all my sins remembered."

    //let text = "To be, or not to be--that is the question:"
    let target = text.ToCharArray();
    let fitness = distance (target)  
    
    let chars =  [|0..127|] |> Array.map char
    let chars = Array.distinct target

    let createPeep () = getRandomSelection target.Length chars

    let terminate bestFit = bestFit = 1.0         

    let breedPeep = breed getMutationRate

    let crossoverPeep = crossover createPeep fitness

    let x =  ga createPeep popSize fitness select breedPeep crossoverPeep terminate 

    printfn "%A" argv
    0 
