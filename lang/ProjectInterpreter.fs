module ProjectInterpreter

open Parser
open ProjectParser
open Library

(* evaluation of AST and eventually checkers *)

(* checker would take an ast, returns ast if good, failwith if bad *)

(* calls function name with the vaariable list vs and initial string s *)
let funcall (name:string) (s:string) (vs:Variable list) =
    match name with
    | "length"       -> toString (length s)
    | "first"        -> first s
    | "last"         -> last s
    | "middle"       -> middle s
    | "isUpper"      -> toString (isUpper s)
    | "isLower"      -> toString (isLower s)
    | "toUpper"      -> toUpper s
    | "toLower"      -> toLower s
    | "isPalindrome" -> toString (isPalindrome s)
    | "reverse"      -> reverse s
    | "repeat"       -> repeat s vs
    | "prepend"      -> prepend s vs
    | "append"       -> append s vs
    | "substring"    -> substring s vs
    | "contains"     -> toString (contains s vs)
    | _              -> failwith "Unrecognized function name"
    
(* evaluates an AST created from the parsers *)
let rec funcEval (ast : Expr) (s:string) : string =
    match ast with
    | Builtin(name,vs) -> funcall name s vs
    | Userdefined -> failwith "Not yet implemented"
    | Seq(e1,e2) -> funcEval e2 (funcEval e1 s)
    | NOP -> s

(* evaluates the AST by calling funcEval with the first string *)
let rec eval (input:string) (ast:Expr) : string =
    match ast with
    | Builtin(name,vs) -> funcall name input vs
    | Userdefined -> failwith "Not yet implemented"
    | Seq(e1,e2) -> eval (eval input e1) e2
    | NOP -> input