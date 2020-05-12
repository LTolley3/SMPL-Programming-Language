﻿// LANGUAGE NAME: 

(*
 *  TODO:
 *     [x] write basic parsers
 *     [x] write main method that prints
 *     [x] support  multiple variables
 *     [x] builtin functions
 *     [x] write an eval function
 *     [x] disallow the empty string as an argument/input
 *     
 *     [ ] handle symbols in strings
 *     [ ] more built in functions: substring, find, replace, substring count, isWord, shuffle
 *     [ ] name the language
 *     [ ] file input
 *     [ ] update spec
 *     [ ] remove parens after functions
 *     
 *     [ ] checker
 *     [ ] anagram?
 *     [ ] concat
 *     [ ] organize library functions
 *     [ ] help menu
 *     [ ] presentation
 *)

open System
open Parser
open ProjectParser
open ProjectInterpreter

[<EntryPoint>]
let main argv =
    if Array.isEmpty argv then
        printfn "\nUsage:\n\t dotnet run \"program\" \n\nwhere program is of the form: string func1 func2 ...\n"
        0
    else
        let input = prepare argv.[0]
        match grammar input with
        | Success(res,_) -> printfn "%A" (eval res)
        | Failure(pos,rule) -> printfn "Invalid Expression"
                               let message = sprintf "Cannot parse input at pos %d in rule '%s': " pos rule
                               let diag = diagnosticMessage 20 pos argv.[0] message
                               printf "%s" diag
        0