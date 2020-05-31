module ProjectParser


open Parser

(* Numbers are non-negative integers    *)
(* Strings are strings                  *)
(* Builtin are builtin functions        *)
(* Sequences are sequences of functions *)
type Expr = 
    | Number of int
    | String of string
    | Builtin of (string * Expr list)
    | Seq of Expr * Expr
    | NOP


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
(* parses strings in either single or double quotes to handle command line stuff *)
let singlequote : Parser<Expr> = pbetween (pchar ''') (pchar ''') pstring |>> String <!> "singlequote"
let doublequote : Parser<Expr> = pbetween (pchar '"') (pchar '"') pstring |>> String <!> "doublequote"
(* pquotes parses a string with double quotes around it *)
let pquotes : Parser<Expr> = doublequote <|> singlequote <!> "pquotes"

(* varlist parses a list of  variables, separated by commas OR a singular variable *)
let arglist : Parser<Expr list> = pseq (pmany0 (pleft expr (pleft (pchar ',') pws0))) expr (fun (vs, v)-> List.append vs [v]) <|> pmany0 expr <!> "arglist"

(* name parses a string, which is later checked to be valid *)
let name : Parser<string> = pmany1 (pletter <|> pdigit <|> pchar '_' <|> pchar '-') |>> stringify 
(* builtinNoParen allows the user to type functions that take no arguments without the parens *)
let builtinNoParen : Parser<Expr> = name |>> (fun n -> Builtin(n,[]))
(* func parses a single function of the form name(variables) *)
let builtin : Parser<Expr> = pseq name (pbetween (pchar '(') (pchar ')') (arglist)) (fun (name, vs) -> Builtin(name, vs)) <!> "builtin"

(* combines many function parsers in sequence *)
let pmany1seq (p : Parser<Expr>) : Parser<Expr> =
    (pmany1 (pleft p pws0)) |>> (fun ps -> ps |> List.fold (fun acc e -> Seq(acc,e)) NOP ) <!> "pmany1seq"
(* sequence parses a sequence of functions *)
let sequence = pseq (builtin <|> builtinNoParen) (pright pws1 expr) (fun (e1,e2) -> Seq(e1,e2)) <!> "sequence"

(* expr parses functions *)
exprImpl := pnum <|> pquotes <|> sequence <|> builtin <|> builtinNoParen <!> "exprImpl"
(* grammar parser that returns an expr *)
let grammar : Parser<Expr> = pleft (pmany1seq expr) peof <!> "grammar"