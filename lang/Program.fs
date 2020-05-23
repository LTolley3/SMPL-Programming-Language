// LANGUAGE NAME: SMPL "simple" string manipulation

open System
open Parser
open ProjectParser
open ProjectInterpreter
open System.IO

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
        let input = argv.[0]
        if input = "help"  && (Array.length argv = 1) then 
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