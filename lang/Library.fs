module Library

open System.IO
open ProjectParser
(* This is the library for all built-in functions. *)

(* length of a string *)
let length (s:string) = 
    String.length s
(* first character in a string *)
let first (s:string) =
    if s = "" then
        ""
    else
        string s.[0]
(* last character in a string *)
let last (s:string) =
    if s = "" then
        ""
    else   
        string s.[(length s) - 1]
(* middle characters of a string *)
let middle (s:string) = 
    if s = "" then
        ""
    else   
        s.[1..(length s)-2]
(* returns the length -1, used in indxeing until the end of a string *)
let getEnd (s:string) =
    if s = "" then 
        0
    else
        (length s) - 1



(* returns true if all letters of s are upper case *)
let isUpper (s:string) = 
    String.forall (fun c -> (int c < 0x61 || int c > 0x7a)) s
(* returns true if all letters of s are lower case *)
let isLower (s:string) =
    String.forall (fun c -> (int c < 0x41 || int c > 0x5a)) s
(* converts lower case letters to upper case letters *)
let toUpper (s:string) =
    String.map (fun c -> 
        if (int c >= 0x61 && int c <= 0x7a) then
            char (int c - 0x20)
        else
            c
    ) s
(* converts upper case letters to lower case letters *)
let toLower (s:string) =
    String.map (fun c -> 
        if (int c >= 0x41 && int c <= 0x5a) then
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
let repeat (s:string) (num : int) = 
    String.replicate num s

(* prepends vs.[0] to string s *)
let prepend (s:string) (prefix : string) =
    prefix + s
(* appends vs.[0] to string s *)
let append (s:string) (suffix : string) =
    s + suffix
(* gets a substring from s *)
let substring (s:string) (first : int) (last : int) =
    try 
        s.[first..last]
    with
        :? System.ArgumentOutOfRangeException -> failwith (sprintf "\nIndex out of range in function 'substring(%d,%d)'." first last)
(* contains checks to see if the string contains a substring, returning true if it does *)
let contains (s:string) (keyword : string) =
    let rec containsHelper source key : bool =
        if (length source) < (length key) then
            false
        else
            if source.[0..((length key)-1)] = key then
                true
            else
                containsHelper (source.[1..]) key
    containsHelper s keyword   
(* replaces all instances of target with replacement *)
let rec replace (s : string) (target : string) (replacement : string) =
    if not (contains s target) then
        s
    else
        if s.[0..((length target)-1)] = target then
            replacement + (replace s.[(length target)..] target replacement)
        else
            (string s.[0]) + (replace s.[1..] target replacement)
(* substringCount returns how many times a substring appears in a string *)
let rec substringCount (s : string) (target : string) =
    if not (contains s target) then
        0
    else
        if s.[0..((length target)-1)] = target then
            1 + (substringCount s.[(length target)..] target)
        else
            substringCount s.[1..] target
(* isWord returns true if s is a word *)
let isWord (s:string) (filename : string) = 
    let words = File.ReadAllLines(filename)
    Array.contains s words
(* shuffle randomizes the characters in s *)
let shuffle (s:string) =
    let r = System.Random()
    let rec shuffleHelper (s:string)  =
        if length s <= 1 then
            s
        else
            let i = r.Next(length s)
            if i = 0 then 
                (string s.[0]) + shuffleHelper s.[1..]
            else
                (string s.[i]) + shuffleHelper (s.[..(i-1)]+s.[i+1..])
    shuffleHelper s