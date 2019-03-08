module FNBitcoin.MiniScriptParser

// TODO: do not use external dependency
open NBitcoin
open System.Text.RegularExpressions
open System

type Policy<'P> =
    | Key of PubKey
    | Multi of uint32 * PubKey[]
    | Hash of uint256
    | Time of uint32
    | Threshold of uint32 * Policy<'P>[]
    | And of Policy<'P> * Policy<'P>
    | Or of Policy<'P> * Policy<'P>
    | AsymmetricOr of Policy<'P> * Policy<'P>

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

type Policy<'T> with
    member this.print() = printPolicy this

// parser
let quoted = Regex(@"\((.*)\)")

let rec (|SurroundedByBrackets|_|) (s: string) =
    let s2  = s.TrimStart()
    let matchC = quoted.Matches(s2)
    if matchC.Count <> 0 then
        let last = matchC.Count
        let lastMatch = matchC.[last - 1]
        printfn "Debug: last match was %O"  lastMatch
        let contentStr = lastMatch.Value.TrimStart('(', ' ').TrimEnd(')', ' ')
        let contents = contentStr.Split(',')
        Some (contents)
    else
        None

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
    // ここから再帰
    | Expression "thres" (SurroundedByBrackets (Threshold thres)) -> Some(Threshold(thres))
    | Expression "and" (SurroundedByBrackets (PubKeysPattern pks)) -> Multi((fst pks), (snd pks)) |> Some
    | Expression "or" (SurroundedByBrackets (PubKeysPattern pks)) -> Multi((fst pks), (snd pks)) |> Some
    | Expression "aor" (SurroundedByBrackets (PubKeysPattern pks)) -> Multi((fst pks), (snd pks)) |> Some
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
