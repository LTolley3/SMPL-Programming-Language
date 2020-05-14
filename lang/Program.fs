// LANGUAGE NAME: SMPL "simple" string manipulation

(*
 *  TODO:
 *     [x] write basic parsers
 *     [x] write main method that prints
 *     [x] support  multiple variables
 *     [x] builtin functions
 *     [x] write an eval function
 *     [x] disallow the empty string as an argument
 *     
 *     [x] handle symbols in strings
 *     [ ] more built in functions: replace, substring count, isWord, shuffle
 *     [x] name the language SMPL
 *     [x] file input
 *     [ ] update spec
 *     [ ] remove parens after functions
 *     
 *     [ ] checker - check variable types, amount, etc.
 *     [ ] anagram?
 *     [ ] concat
 *     [ ] organize library functions
 *     [ ] help menu 
                single quoted strings to ignore bash characters (or set +H)
 *     [ ] presentation
 *)

open System
open Parser
open ProjectParser
open ProjectInterpreter
open System.IO

(* readInput will read a file if a .txt file is supplied *)
let readInput (input : string) =
    match Path.GetExtension(input) with
        | ".txt" -> System.IO.File.ReadAllText input
        | _       -> input
(* readSMPL will read a program file if a .smpl is supplied *)
let readSMPL (input : string) = 
    match Path.GetExtension(input) with
        | ".smpl" -> System.IO.File.ReadAllText input
        | _       -> input

[<EntryPoint>]
let main argv =
    if Array.isEmpty argv then
        printfn "\nUsage:\n\t dotnet run \"input\" \"program\" \n\nwhere input is a string and program is of the form: func1 func2 ...\n"
        0
    else
        let input = readInput argv.[0]
        let program = readSMPL argv.[1]
        match grammar (prepare program) with
        | Success(res,_) -> printfn "%A" (eval input res)
        | Failure(pos,rule) -> printfn "Invalid Expression"
                               let message = sprintf "Cannot parse input at pos %d in rule '%s': " pos rule
                               let diag = diagnosticMessage 20 pos program message
                               printf "%s" diag
        0