module ProjectParser


open Parser

(* Variables for arguments of functions *)
type Variable = 
    | String of string
    | Number of int

(* Builtin are builtin functions, not defined by user *)
(* Sequences are sequences of functions               *)
(* Userdefined are user defined functions, tbi        *)
type Expr = 
    | Builtin of (string * Variable list)
    | Userdefined //TODO add user defined function support
    | Seq of Expr * Expr
    | NOP

(* Start is the start of the program, with a master string and expr *)
type Start = string * Expr 
    


(* List of all built in funtions *)
let FunctionList = ["length"; "first"; "last"; "middle"; "isUpper"; "isLower";
                    "toUpper"; "toLower"; "isPalindrome"; "reverse"; "repeat"; 
                    "prepend"; "append"; "substring"]



(* Parsers *)

(* expr, exprImpl is the recursive initializing of the expr parser *)
let expr, exprImpl = recparser()
(* pnum parses numbers that would be in function arguments *)
let pnum : Parser<Variable> = (pmany1 pdigit) |>> stringify |>> int |>> Number <!> "pnum"
(* pstring parses strings that would be in function arguments TODO add symbol support, support empty string *)
let pstring : Parser<string> = (pmany1 pletter) |>> stringify <!> "pstring"
(* variable parses variables that are in function arguments *)
let variable : Parser<Variable> = pnum <|> (pstring |>> String) <!> "variable"
(* varlist parses a list of  variables, separated by commas OR a singular variable *)
let varlist : Parser<Variable list> = pseq (pmany0 (pleft variable (pchar ','))) variable (fun (vs, v)-> List.append vs [v]) <|> pmany0 variable

(* name parses a valid function name *)
let name (i : Input) : Outcome<string> = 
    match pstring i with //get just the intial string
    | Success(str,rest) -> 
        if List.contains str FunctionList then
            Success(str, rest)
        else
            Failure(position i, "pname")
    | Failure(pos, name) -> Failure(pos, name)

(* func parses a single function of the form name(variables) *)
let builtin : Parser<Expr> = pseq name (pbetween (pchar '(') (pchar ')') (varlist)) (fun (name, vs) -> Builtin(name, vs)) <!> "builtin"

(* combines many function parsers in sequence *)
let pmany1seq (p : Parser<Expr>) : Parser<Expr> =
    (pmany1 (pleft p pws0)) |>> (fun ps -> ps |> List.fold (fun acc e -> Seq(acc,e)) NOP ) <!> "pmany1seq"

(* expr parses functions, TODO add user defined function support *)
exprImpl := builtin <!> "exprImpl"
(* grammar parser that returns a Start *)
let grammar : Parser<Start> = pleft (pseq (pseq pstring pws1 (fun (s,sp) -> s)) (pmany1seq expr) id) peof <!> "grammar"

