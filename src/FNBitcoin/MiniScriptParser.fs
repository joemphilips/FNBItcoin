module FNBitcoin.MiniScriptParser

open NBitcoin
open System.Text.RegularExpressions
open System

type Policy =
    | Key of PubKey
    | Multi of uint32 * PubKey[]
    | Hash of uint256
    | Time of uint32
    | Threshold of uint32 * Policy[]
    | And of Policy * Policy
    | Or of Policy * Policy
    | AsymmetricOr of Policy * Policy

// printer
let rec printPolicy p =
    match p with
    | Key k1 -> sprintf "pk(%s)" (string (k1.ToHex()))
    | Multi (m, klist) -> klist
                          |> Seq.map(fun k -> string (k.ToHex()))
                          |> Seq.reduce(fun a b -> sprintf "%s,%s" a b)
                          |> sprintf "multi(%d,%s)" m
    | Hash h -> sprintf "hash(%s)" (string (h.ToString()))
    | Time t -> sprintf "time(%s)" (string (t.ToString()))
    | Threshold (m, plist) -> plist
                              |> Array.map(printPolicy)
                              |> Array.reduce(fun a b -> sprintf "%s,%s" a b)
                              |> sprintf "thres(%d,%s)" m
    | And (p1, p2) -> sprintf "and(%s,%s)" (printPolicy p1) (printPolicy p2)
    | Or (p1, p2) ->  sprintf "or(%s,%s)" (printPolicy p1) (printPolicy p2)
    | AsymmetricOr (p1, p2) ->  sprintf "aor(%s,%s)" (printPolicy p1) (printPolicy p2)

type Policy with
    member this.print() = printPolicy this

// parser
let quoted = Regex(@"\((.*)\)")

// TODO: this has bug in case of nested expression. Fix.
let rec (|SurroundedByBrackets|_|) (s: string) =
    let s2  = s.TrimStart()
    let matchC = quoted.Matches(s2)
    if matchC.Count = 0 then
        None
    else
        let last = matchC.Count
        let lastMatch = matchC.[last - 1]
        printfn "Debug: last match was %O"  lastMatch
        let contentStr = lastMatch.Value.TrimStart('(', ' ').TrimEnd(')', ' ')
        let contents = contentStr.Split(',')
        Some (contents)

let (|Expression|_|) (prefix: string) (s: string) =
    let s = s.TrimStart()
    if s.StartsWith(prefix) then
         Some (s.Substring(prefix.Length))
    else
        None

let (|PubKeyPattern|_|) (s: string[]) =
    if 1 < s.Length then
        None
    else
        let s = s.[0]
        try
            Some(PubKey(s))
        with
            | :? FormatException as ex ->  None

let (|PubKeysPattern|_|) (s: string[]) =
    if s.Length < 2 then
        None
    else
        let m = s.[0]
        match UInt32.TryParse(m) with
        | (false, _) -> None
        | (true, i) ->
            try
                let pks = s.[1..s.Length] |> Array.map(fun hex -> PubKey(hex))
                Some (i, pks)
            with
            | :?FormatException -> None

let (|Hash|_|) (s: string[]) =
    if 1 < s.Length then
        None
    else
        Some(uint256(s.[0]))

let (|Time|_|) (s: string[]) =
    if 1 < s.Length then
        None
    else
        Some(uint32(s.[0]))


let rec (|Policy|_|) s =
    match s with
    | Expression "pk" (SurroundedByBrackets (PubKeyPattern pk)) -> Some (Key pk)
    | Expression "multi" (SurroundedByBrackets (PubKeysPattern pks)) -> Multi((fst pks), (snd pks)) |> Some
    | Expression "hash" (SurroundedByBrackets (Hash hash)) -> Some(Hash hash)
    | Expression "time" (SurroundedByBrackets (Time t)) -> Some(Time(t))
    // recursive matches
    | Expression "thres" (SurroundedByBrackets (Threshold thres)) -> Some(Threshold(thres))
    | Expression "and" (SurroundedByBrackets (And (expr1, expr2))) -> And(expr1, expr2) |> Some
    | Expression "or" (SurroundedByBrackets (Or (expr1, expr2))) -> Or(expr1, expr2) |> Some
    | Expression "aor" (SurroundedByBrackets (AsymmetricOr (expr1, expr2))) -> AsymmetricOr(expr1, expr2) |> Some
    | _ -> None
and (|Threshold|_|) (s: string[]) =
    if s.Length < 2 then
        None
    else
        let thresholdStr = s.[0]
        match UInt32.TryParse(thresholdStr) with
        | (true, threshold) ->
            let subPolicy = s.[1..s.Length] |> Array.choose((|Policy|_|))
            if subPolicy.Length <> s.Length then
                None
            else
                Some (threshold, subPolicy)
        | (false, _) -> None
and (|And|_|) (s: string[]) = twoSubExpressions s
and (|Or|_|) (s: string[]) = twoSubExpressions s
and (|AsymmetricOr|_|) (s: string[]) = twoSubExpressions s
and twoSubExpressions (s: string[]) =
    if s.Length <> 2 then
        None
    else
        let subPolicies = s |> Array.choose((|Policy|_|))
        if subPolicies.Length <> s.Length then
            None
        else
            Some (subPolicies.[0], subPolicies.[1])
