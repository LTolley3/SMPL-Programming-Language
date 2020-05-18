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
                    "prepend"; "append"; "substring"; "contains"; "replace"; "substringCount"]

(* Parsers *)

(* expr, exprImpl is the recursive initializing of the expr parser *)
let expr, exprImpl = recparser()
(* pnum parses numbers that would be in function arguments *)
let pnum : Parser<Expr> = (pmany1 pdigit) |>> stringify |>> int |>> Number <!> "pnum"
(* psymbol parses any number or symbol in the ASCII table *)
let psymbol : Parser<char> = (pchar '!') <|> (pchar '#') <|> (pchar '$') <|> (pchar '%') <|> (pchar '&') <|> (pchar '(') <|> (pchar ')') <|>
                             (pchar '*') <|> (pchar '+') <|> (pchar ',') <|> (pchar '-') <|> (pchar '.') <|> (pchar '/') <|> (pchar '[') <|> 
                             (pchar '\\') <|> (pchar ']') <|> (pchar '^') <|> (pchar '_') <|> (pchar '`') <|> (pchar '{') <|> (pchar '}') <|> 
                             (pchar '~') <|> (pchar ' ')
let pstring : Parser<string> = pmany1 (pletter <|> pdigit <|> psymbol) |>> stringify <!> "pstring"
(*parses strings in either single or double quotes to handle command line shenannigans *)
let singlequote : Parser<Expr> = pbetween (pchar ''') (pchar ''') pstring |>> String <!> "singlequote"
let doublequote : Parser<Expr> = pbetween (pchar '"') (pchar '"') pstring |>> String <!> "doublequote"
(* pquotes parses a string with double quotes around it *)
let pquotes : Parser<Expr> = doublequote <|> singlequote <!> "pquotes"

(* varlist parses a list of  variables, separated by commas OR a singular variable *)
let arglist : Parser<Expr list> = pseq (pmany0 (pleft expr (pleft (pchar ',') pws0))) expr (fun (vs, v)-> List.append vs [v]) <|> pmany0 expr <!> "arglist"

(* name parses a valid function name  TODO this should be part of the checker, in its place would just be a palphastring*)
let name : Parser<string> = pmany1 (pletter <|> pdigit <|> pchar '_' <|> pchar '-') |>> stringify 
(*    match pstring i with //get just the intial string
    | Success(str,rest) -> 
        if List.contains str FunctionList then
            Success(str, rest)
        else
            Failure(position i, "pname")
    | Failure(pos, name) -> Failure(pos, name)
*)

(* builtingNoParen allows the user to type functions that take no arguments without the parens *)
let builtinNoParen : Parser<Expr> = name |>> (fun n -> Builtin(n,[]))
(* func parses a single function of the form name(variables) *)
let builtin : Parser<Expr> = pseq name (pbetween (pchar '(') (pchar ')') (arglist)) (fun (name, vs) -> Builtin(name, vs)) <!> "builtin"

(* combines many function parsers in sequence *)
let pmany1seq (p : Parser<Expr>) : Parser<Expr> =
    (pmany1 (pleft p pws0)) |>> (fun ps -> ps |> List.fold (fun acc e -> Seq(acc,e)) NOP ) <!> "pmany1seq"
(* sequence parses a sequence of functions *)
let sequence = pseq builtin (pright pws1 expr) (fun (e1,e2) -> Seq(e1,e2)) <!> "sequence"

(* expr parses functions, TODO add user defined function support *)
exprImpl := pnum <|> pquotes <|> sequence <|> builtin <|> builtinNoParen <!> "exprImpl"
(* grammar parser that returns an expr *)
let grammar : Parser<Expr> = pleft (pmany1seq expr) peof <!> "grammar"