module ProjectInterpreter

open Parser
open ProjectParser
open Library

(* evaluation of AST and eventually checkers *)

(* checker would take an ast, returns ast if good, failwith if bad *)

(* calls function name with the vaariable list vs and initial string s *)
let funcall (name:string) (s:string) (args: string list) : Expr=
    match name with
    | "length"       -> length s |> Number
    | "first"        -> first s |> String
    | "last"         -> last s |> String
    | "middle"       -> middle s |> String
    | "getEnd"       -> getEnd s |> Number
    | "isUpper"      -> string (isUpper s) |> String
    | "isLower"      -> string (isLower s) |> String
    | "toUpper"      -> toUpper s |> String
    | "toLower"      -> toLower s |> String
    | "isPalindrome" -> string (isPalindrome s) |> String
    | "reverse"      -> reverse s |> String
    | "repeat"       -> repeat s (int args.[0]) |> String
    | "prepend"      -> prepend s (args.[0]) |> String
    | "append"       -> append s (args.[0]) |> String
    | "substring"    -> substring s (int (args.[0])) (int (args.[1])) |> String
    | "contains"     -> string (contains s (args.[0])) |> String
    | _              -> failwith "Unrecognized function name"
    

(* evaluates the AST by calling funcEval with the first string *)
let rec eval (input:string) (ast:Expr) : string =
    match ast with
    | Number(n) -> string n
    | String(s) -> s
    | Builtin(name,es) -> 
            (* evaluate each argument of the function, then call *)
            let args = List.map (fun e -> eval input e) es
            eval input (funcall name input args)
    | Userdefined -> failwith "Not yet implemented"
    | Seq(e1,e2) -> eval (eval input e1) e2
    | NOP -> input