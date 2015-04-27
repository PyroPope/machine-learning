module MnistData

open System.IO
open ANN

let minstSamples =
    let buildAnswerList count index  =
        [for i in 0.0..(count - 1.0) -> if i = index then 1.0 else 0.0]
    let fileName =  "training-20000.csv"
    let trainFile = __SOURCE_DIRECTORY__ + @"\digits\" + fileName;
    (File.ReadAllLines trainFile) 
    |> Array.map (fun line -> line.Split(',')) |> Array.toList
    |> List.map (fun fields -> fields |> Array.toList |> List.map (float))        
    |> List.map (fun vals ->
        {input=vals.Tail; target=(buildAnswerList 10.0 vals.Head)})


