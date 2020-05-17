module ProjectParser


open Parser


(* Builtin are builtin functions, not defined by user *)
(* Sequences are sequences of functions               *)
(* Userdefined are user defined functions, tbi        *)
type Expr = 
    | Number of int
    | String of string
    | Builtin of (string * Expr list)
    | Userdefined //TODO add user defined function support
    | Seq of Expr * Expr
    | NOP


(* List of all built in funtions *)
let FunctionList = ["length"; "first"; "last"; "middle"; "getEnd"; "isUpper"; "isLower";
                    "toUpper"; "toLower"; "isPalindrome"; "reverse"; "repeat"; 
                    "prepend"; "append"; "substring"; "contains"]


(* Parsers *)

(* expr, exprImpl is the recursive initializing of the expr parser *)
let expr, exprImpl = recparser()
(* pnum parses numbers that would be in function arguments *)
let pnum : Parser<Expr> = (pmany1 pdigit) |>> stringify |>> int |>> Number <!> "pnum"

(* palphastring parses strings that would be in function arguments *)
let palphastring : Parser<string> = (pmany1 (pletter <|> (pchar ' '))) |>> stringify <!> "palphastring"
(*parses strings in either single or double quotes to handle command line shenannigans *)
let singlequote : Parser<Expr> = pbetween (pchar ''') (pchar ''') (palphastring) |>> String <!> "singlequote"
let doublequote : Parser<Expr> = pbetween (pchar '"') (pchar '"') (palphastring) |>> String <!> "doublequote"
(* pquotes parses a string with double quotes around it *)
let pquotes : Parser<Expr> = doublequote <|> singlequote <!> "pquotes"

(* varlist parses a list of  variables, separated by commas OR a singular variable *)
let arglist : Parser<Expr list> = pseq (pmany0 (pleft expr (pchar ','))) expr (fun (vs, v)-> List.append vs [v]) <|> pmany0 expr <!> "arglist"

(* name parses a valid function name  TODO this should be part of the checker, in its place would just be a palphastring*)
let name (i : Input) : Outcome<string> = 
    match palphastring i with //get just the intial string
    | Success(str,rest) -> 
        if List.contains str FunctionList then
            Success(str, rest)
        else
            Failure(position i, "pname")
    | Failure(pos, name) -> Failure(pos, name)

(* func parses a single function of the form name(variables) *)
let builtin : Parser<Expr> = pseq name (pbetween (pchar '(') (pchar ')') (arglist)) (fun (name, vs) -> Builtin(name, vs)) <!> "builtin"

(* combines many function parsers in sequence *)
let pmany1seq (p : Parser<Expr>) : Parser<Expr> =
    (pmany1 (pleft p pws0)) |>> (fun ps -> ps |> List.fold (fun acc e -> Seq(acc,e)) NOP ) <!> "pmany1seq"
(* sequence parses a sequence of functions *)
let sequence = pseq builtin (pright pws1 expr) (fun (e1,e2) -> Seq(e1,e2)) <!> "sequence"

(* expr parses functions, TODO add user defined function support *)
exprImpl := pnum <|> pquotes <|> sequence <|> builtin <!> "exprImpl"
(* grammar parser that returns an expr *)
let grammar : Parser<Expr> = pleft (pmany1seq expr) peof <!> "grammar"

