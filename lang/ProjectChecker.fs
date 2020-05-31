module ProjectChecker

open ProjectParser
open Library

 (* checks to make sure function 'name' has exactly n arguments *)
let checkArgNum (n : int) (name : string) (args : string list) : unit =
    if List.isEmpty args then
        if n <> 0 then
            let msg : string = sprintf "\nInvalid number of arguments in function '%s'. Expected %d, found 0." name n
            failwith msg
    else
        if n <> List.length args then
            let msg : string = sprintf "\nInvalid number of arguments in function '%s'. Expected %d, found %d." name n (List.length args)
            failwith msg
(* checks to make sure argument 'arg' is an int *)
let checkArgInt (name : string) (arg: string) : unit =
    try 
        int arg |> ignore
    with
        :? System.FormatException -> 
            let msg = sprintf "\nInvalid argument type of '%s' in function '%s': Expected number, found string." arg name
            failwith msg
(* checks to make sure functions are called with correct arguments *)
let functionCheck (name : string) (args : string list) : unit =
    match name with
    | "length"         -> checkArgNum 0 name args
    | "first"          -> checkArgNum 0 name args
    | "last"           -> checkArgNum 0 name args
    | "middle"         -> checkArgNum 0 name args
    | "getEnd"         -> checkArgNum 0 name args
    | "isUpper"        -> checkArgNum 0 name args
    | "isLower"        -> checkArgNum 0 name args
    | "toUpper"        -> checkArgNum 0 name args
    | "toLower"        -> checkArgNum 0 name args
    | "isPalindrome"   -> checkArgNum 0 name args
    | "reverse"        -> checkArgNum 0 name args
    | "repeat"         -> checkArgNum 1 name args; checkArgInt name args.[0]
    | "prepend"        -> checkArgNum 1 name args
    | "append"         -> checkArgNum 1 name args
    | "substring"      -> checkArgNum 2 name args; checkArgInt name args.[0]; checkArgInt name args.[1]
    | "contains"       -> checkArgNum 1 name args
    | "replace"        -> checkArgNum 2 name args
    | "substringCount" -> checkArgNum 1 name args
    | "isWord"         -> checkArgNum 1 name args
    | "shuffle"        -> checkArgNum 0 name args
    | _                -> failwith (sprintf "\nUnrecognized function name: '%s'" name)