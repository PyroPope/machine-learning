module File1


let rec rtt start finish b (state: string) =
    let ok, newState = b start state
    match ok with
    | false -> rtt start finish b newState
    | true  -> 
        match start with
        | s when s = finish -> ()
        | _ -> 
            let s = min finish (start + (max 2 (start/100)))
            printfn "new s %d" s
            rtt s finish b newState

let fn count state =
    printfn "%d %s" count state ;
    string (int state - 1)

let e state = int state
 
let b e f count state =
    printfn "count: %d    state: %s" count state
    let newState : string = f count state
    let evaluation = e newState
    let ok = 10 + count + evaluation < 1
    printfn "%b" ok
    System.Threading.Thread.Sleep(1000)
    (ok, newState)

let goRtt() = rtt 0 10 (b e fn) "0"
    
