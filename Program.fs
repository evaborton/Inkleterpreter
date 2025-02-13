open Combinator
open AST
open Parser
open Evaluator
open System
open System.IO

[<EntryPoint>]
let main args = 
    
    if args.Length <> 1 then
        printfn "Usage: dotnet run <path>"
        exit 1

    let path = args[0]
    let text = IO.File.ReadAllText path

    match parse text with
    | Some wv -> 
        match wv with 
        | pt, bgc, ptc -> 
            let x = eval pt bgc ptc
            File.WriteAllText("weaving.html", x)
    | None -> printfn "Invalid program."

    0