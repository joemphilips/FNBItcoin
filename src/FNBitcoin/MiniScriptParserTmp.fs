module MiniscriptParserTmp

open System.Text.RegularExpressions
open NBitcoin

// https://fpish.net/blog/adam.granicz/id/1024/http~3a~2f~2fwww.intellifactory.com~2fblogs~2fadam.granicz~2f2009~2f5~2f23~2fParsing-with-active-patterns.article
let matchToken pattern s =
    let res =
        Regex.Match
            (s, pattern |> sprintf "\A(%s)((?s).*)", RegexOptions.Multiline)
    if res.Success then (res.Groups.[1].Value, res.Groups.[2].Value) |> Some
    else None

let (|WS|_|) s = matchToken "[ |\t|\n|\r\n]+" s
let (|COMMENT|_|) s = matchToken "#.*[\n|\r\n]" s

let (|WHITESPACE|_|) s =
    match s with
    | WS rest -> rest |> Some
    | COMMENT rest -> rest |> Some
    | _ -> None

let rec (|Star|_|) f acc s =
    match f s with
    | Some(res, rest) -> (|Star|_|) f (res :: acc) rest
    | None -> (acc, s) |> Some

let (|WhiteSpace|_|) s = (|Star|_|) (|WHITESPACE|_|) [] s

let rec MatchTokenNoWS s pattern =
    match (|WhiteSpace|_|) s with
    | Some(_, rest) -> rest |> matchToken pattern
    | None -> matchToken pattern s

let MatchToken s f pattern =
    pattern
    |> MatchTokenNoWS s
    |> Option.bind f

let MatchSymbol s pattern =
    pattern |> MatchToken s (fun (_, rest) -> rest |> Some)

type Expr =
    | BoolAnd of Expr * Expr
    | BoolOr of Expr * Expr
    | Add of Expr * Expr
    | Equal of Expr * Expr
    | EqualVerify of Expr * Expr
    | CheckSig of Expr
    | CheckSigVerify of Expr
    | If of bool
    | Number of uint32
    | Hash160 of uint160
    | Hash256 of uint256
    | PubKey of byte []

let (|NUMBER|_|) s = "[0-9]+\.?[0-9]"
                     |> MatchToken
                     <| s
