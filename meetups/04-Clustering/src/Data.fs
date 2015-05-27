module Data

open System
open System.IO
open System.Text.RegularExpressions

let private getChildDirectory childName = 
    let appRoot = DirectoryInfo(AppDomain.CurrentDomain.BaseDirectory).Parent.Parent.Parent
    Path.Combine(appRoot.FullName, childName)
    |> Directory.CreateDirectory
    |> (fun di -> di.FullName)

let dataDir = getChildDirectory "data"


let readLines filename = 
    let skipLineRgx = new Regex(@"(^\s*$|^\s*#)", RegexOptions.Compiled)
    Path.Combine(dataDir, filename)
        |> File.ReadAllLines
        //|> Array.filter (fun line -> not (skipLineRgx.IsMatch(line)))
        |> Array.filter ((skipLineRgx.IsMatch) >> not)
    
    
let readRows filename =
    let splitRgx = new Regex(@"\s*[,]\s*", RegexOptions.Compiled)
    readLines filename
        |> Array.map (splitRgx.Split)

        
