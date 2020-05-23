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
 *     [x] more built in functions: replace, substring count, isWord, shuffle
 *     [x] name the language SMPL
 *     [x] file input
 *     [x] update spec
 *     [x] remove parens after functions
 *     [x] parse any string (including symbols) as arguments
 *     
 *     [x] nested functions
 *     [x] checker - check variable types, amount, etc.
 *     [/] anagram?
 *     [x] concat? (any concat of functions is possible with nested functions!)
 *     [/] organize library functions
 *     [x] test files
 *     [ ] update spec (title, string  literal, remaining work)
 *     [ ] TRANSFER.txt
 *     [x] help menu 
 *              give an example
 *              single quoted strings to ignore bash characters (or set +H)
 *     [ ] remove all debug comments
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

(* help menu displays useful information about using smpl *)
let helpMenu : unit = 
    printfn "\nUsage:\n\t dotnet run \"input\" \"program\" \n\nwhere input is a string and program is of the form: func1 func2 ... or a .smpl file.\n"
    printfn "Example program:\n\n\tdotnet run \"example_input\" \"reverse append('ABC')\"\n"
    printfn "Output: tupni_elpmaxeABC"
    printfn "\nNote that if the shell is misreading double quoted strings, try using single quotes or typing '$ set +H'\n"
    printfn "\n-=-=-=- LIST OF BUILT-IN FUNCTIONS -=-=-=-"
    printfn "Checking properties:\nlength\nisUpper\nisLower\nisPalindrome\ncontains('target')\nisWord('dictionary filepath')\nsubstringCount('target')\n"
    printfn "Getter methods:\nfirst\nlast\nmiddle\ngetEnd\nsubstring(index1, index2)\n"
    printfn "Modifier methods:\ntoUpper\ntoLower\nrepeat(count)\nreplace('target', 'replacement')\nreverse\nprepend('str')\nappend('str')\nshuffle"

[<EntryPoint>]
let main argv =
    if Array.isEmpty argv then
        printfn "\nUsage:\n\t dotnet run \"input\" \"program\" \n\nwhere input is a string and program is of the form: func1 func2 ... or a .smpl file.\n\nFor more information, run 'dotnet run \"help\"'\n"
        0
    else
        let input = readInput argv.[0]
        if input = "help" then 
            helpMenu
            0
        else
            let program = readSMPL argv.[1]
            match grammar (prepare program) with
            | Success(res,_) -> printfn "%A" (eval input res)
            | Failure(pos,rule) -> printfn "\nInvalid Expression."
                                   let message = sprintf "Cannot parse input at pos %d in rule '%s': " pos rule
                                   let diag = diagnosticMessage 20 pos program message
                                   printf "%s" diag
            0