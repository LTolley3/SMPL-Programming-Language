module Library

open ProjectParser
(* This is the library for all built-in functions. *)

let toString (s:'a) =
    System.Convert.ToString s
(* length of a string *)
let length (s:string) = 
    String.length s
(* first character in a string *)
let first (s:string) =
    if s = "" then
        ""
    else
        System.Convert.ToString s.[0]
(* last character in a string *)
let last (s:string) =
    if s = "" then
        ""
    else   
        System.Convert.ToString s.[(String.length s) - 1]
(* middle characters of a string *)
let middle (s:string) = 
    if s = "" then
        ""
    else   
        s.[1..(length s)-2]



(* returns true if all letters of s are upper case *)
let isUpper (s:string) = 
    String.forall (fun c -> (int c < 0x61 || int c > 0x7a)) s
(* returns true if all letters of s are lower case *)
let isLower (s:string) =
    String.forall (fun c -> (int c < 0x41 || int c > 0x5a)) s
(* converts lower case letters to upper case letters *)
let toUpper (s:string) =
    String.map (fun c -> 
        if (int c >= 0x61 || int c <= 0x7a) then
            char (int c - 0x20)
        else
            c
    ) s
(* converts upper case letters to lower case letters *)
let toLower (s:string) =
    String.map (fun c -> 
        if (int c >= 0x41 || int c <= 0x5a) then
            char (int c + 0x20)
        else
            c
    ) s
(* returns if the string is a palindrome *)
let rec isPalindrome (s:string) = 
    if length s <= 1 then
        true
    else
        let x,y = (first s),(last s)
        if x = y then
            isPalindrome (middle s)
        else
            false
(* reverses a string *)
let rec reverse (s:string) =
    if (length s <= 1) then
        s
    else
        (last s) + (reverse (middle s)) + (first s)
(* repeats a string s n number of times, returns concatenated string *)
let repeat (s:string) (vs:Variable list) = 
    if (List.isEmpty vs) || (List.length vs <> 1) then
        failwith "Invalid number of arguments. Expecting 1."
    else
        match vs.[0] with
        | Number(n) -> String.replicate n s
        | String(_) -> failwith "Invalid function argument type. Expected int, found string"
(* prepends vs.[0] to string s *)
let prepend (s:string) (vs:Variable list) =
    if (List.isEmpty vs) || (List.length vs <> 1) then
        failwith "Invalid number of arguments. Expecting 1."
    else
        match vs.[0] with
        | String(p) -> p + s
        | Number(_) -> failwith "Invalid function argument type. Expected string, found int"
(* appends vs.[0] to string s *)
let append (s:string) (vs:Variable list) =
    if (List.isEmpty vs) || (List.length vs <> 1) then
        failwith "Invalid number of arguments. Expecting 1."
    else
        match vs.[0] with
        | String(p) -> s + p
        | Number(_) -> failwith "Invalid function argument type. Expected string, found int"
(* gets a substring from s *)
let substring (s:string) (vs:Variable list) =
    if (List.isEmpty vs) || (List.length vs <> 2) then
        failwith "Invalid number of arguments. Expecting 2."
    else
        match (vs.[0], vs.[1]) with
        | Number(n1),Number(n2) -> s.[n1..n2]     
        | _                     -> failwith "Invalid function argument type. Expected int, int."

(* contains checks to see if the string contains a substring, returning true if it does *)
let contains (s:string) (vs:Variable list) =
    if (List.isEmpty vs) || (List.length vs <> 1) then
        failwith "Invalid number of arguments. Expecting 1."
    else
        let rec containsHelper source sub : bool =
            if (length source) < (length sub) then
                false
            else
                if source.[0..((length sub)-1)] = sub then
                    true
                else
                    containsHelper (source.[1..]) sub

        match vs.[0] with
        | String(sub) -> containsHelper s sub
        | Number(_) -> failwith "Invalid function argument type. Expected string, found int"