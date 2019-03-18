module FNBitcoin.MiniScriptDecompiler

open NBitcoin
open System
open FNBitcoin.Utils.Parser
open FNBitcoin.Utils
open MiniScriptAST
open Microsoft.FSharp.Reflection
/// Subset of Bitcoin Script which is used in Miniscript
type Token =
    | BoolAnd
    | BoolOr
    | Add
    | Equal
    | EqualVerify
    | CheckSig
    | CheckSigVerify
    | CheckMultiSig
    | CheckMultiSigVerify
    | CheckSequenceVerify
    | FromAltStack
    | ToAltStack
    | Drop
    | Dup
    | If
    | IfDup
    | NotIf
    | Else
    | EndIf
    | ZeroNotEqual
    | Size
    | Swap
    | Tuck
    | Verify
    | Hash160
    | Sha256
    | Number of uint32
    | Hash160Hash of uint160
    | Sha256Hash of uint256
    | Pk of NBitcoin.PubKey
    | Any

type TokenCategory =
    | BoolAnd
    | BoolOr
    | Add
    | Equal
    | EqualVerify
    | CheckSig
    | CheckSigVerify
    | CheckMultiSig
    | CheckMultiSigVerify
    | CheckSequenceVerify
    | FromAltStack
    | ToAltStack
    | Drop
    | Dup
    | If
    | IfDup
    | NotIf
    | Else
    | EndIf
    | ZeroNotEqual
    | Size
    | Swap
    | Tuck
    | Verify
    | Hash160
    | Sha256
    | Number
    | Hash160Hash
    | Sha256Hash
    | Pk
    | Any

type ParseException(msg, ex : exn) =
    inherit Exception(msg, ex)
    new(msg) = ParseException(msg, null)

type Token with
    member this.GetItem() =
        match this with
        | Number n -> box n |> Some
        | Hash160Hash h -> box h |> Some
        | Sha256Hash h -> box h |> Some
        | Pk pk -> box pk |> Some
        | _ -> None
    member this.GetItemUnsafe() =
        match this with
        | Number n -> n :> obj
        | Hash160Hash h -> h :> obj
        | Sha256Hash h -> h :> obj
        | Pk pk -> pk :> obj
        | i -> failwith (sprintf "failed to get item from %A" i)

    // usual reflection is not working for extracing name of each case. So we need this.
    member this.GetCategory() =
        match this with
        | BoolAnd -> TokenCategory.BoolAnd
        | BoolOr -> TokenCategory.BoolOr
        | Add -> TokenCategory.Add
        | Equal -> TokenCategory.Equal
        | EqualVerify -> TokenCategory.EqualVerify
        | CheckSig -> TokenCategory.CheckSig
        | CheckSigVerify -> TokenCategory.CheckSigVerify
        | CheckMultiSig -> TokenCategory.CheckMultiSig
        | CheckMultiSigVerify -> TokenCategory.CheckMultiSigVerify
        | CheckSequenceVerify -> TokenCategory.CheckSequenceVerify
        | FromAltStack -> TokenCategory.FromAltStack
        | ToAltStack -> TokenCategory.ToAltStack
        | Drop -> TokenCategory.Drop
        | Dup -> TokenCategory.Dup
        | If -> TokenCategory.If
        | IfDup -> TokenCategory.IfDup
        | NotIf -> TokenCategory.NotIf
        | Else -> TokenCategory.Else
        | EndIf -> TokenCategory.EndIf
        | ZeroNotEqual -> TokenCategory.ZeroNotEqual
        | Size -> TokenCategory.Size
        | Swap -> TokenCategory.Swap
        | Tuck -> TokenCategory.Tuck
        | Verify -> TokenCategory.Verify
        | Hash160 -> TokenCategory.Hash160
        | Sha256 -> TokenCategory.Sha256
        | Number _ -> TokenCategory.Number
        | Hash160Hash _ -> TokenCategory.Number
        | Sha256Hash _ -> TokenCategory.Sha256Hash
        | Pk _ -> TokenCategory.Pk
        | Any -> TokenCategory.Any

let private tryGetItemFromOp (op: Op) =
    let size = op.PushData.Length
    match size with
    | 20 -> Ok(Token.Hash160Hash(uint160 (op.PushData)))
    | 32 -> Ok(Token.Sha256Hash(uint256 (op.PushData)))
    | 33 -> 
        try 
            Ok(Token.Pk(NBitcoin.PubKey(op.PushData)))
        with :? FormatException as ex -> 
            Error(ParseException("Invalid Public Key", ex))
    | _ -> 
        match op.GetInt().HasValue with
        | true -> 
            let v = op.GetInt().Value
            /// no need to check v >= 0 since it is checked in NBitcoin side
            Ok(Token.Number(uint32 v))
        | false -> 
            Error(ParseException(sprintf "Invalid push with Opcode %O" op))

let private castOpToToken (op : Op) : Result<Token, ParseException> =
    match (op.Code) with
    | OpcodeType.OP_BOOLAND -> Ok(Token.BoolAnd)
    | OpcodeType.OP_BOOLOR -> Ok(Token.BoolOr)
    | OpcodeType.OP_EQUAL -> Ok(Token.Equal)
    | OpcodeType.OP_EQUALVERIFY -> Ok(Token.EqualVerify)
    | OpcodeType.OP_CHECKSIG -> Ok(Token.CheckSig)
    | OpcodeType.OP_CHECKSIGVERIFY -> Ok(Token.CheckSigVerify)
    | OpcodeType.OP_CHECKMULTISIG -> Ok(Token.CheckMultiSig)
    | OpcodeType.OP_CHECKMULTISIGVERIFY -> Ok(Token.CheckMultiSigVerify)
    | OpcodeType.OP_CHECKSEQUENCEVERIFY -> Ok(Token.CheckSequenceVerify)
    | OpcodeType.OP_FROMALTSTACK -> Ok(Token.FromAltStack)
    | OpcodeType.OP_TOALTSTACK -> Ok(Token.ToAltStack)
    | OpcodeType.OP_DROP -> Ok(Token.Drop)
    | OpcodeType.OP_DUP -> Ok(Token.Dup)
    | OpcodeType.OP_IF -> Ok(Token.If)
    | OpcodeType.OP_IFDUP -> Ok(Token.IfDup)
    | OpcodeType.OP_NOTIF -> Ok(Token.NotIf)
    | OpcodeType.OP_ELSE -> Ok(Token.Else)
    | OpcodeType.OP_ENDIF -> Ok(Token.EndIf)
    | OpcodeType.OP_0NOTEQUAL -> Ok(Token.ZeroNotEqual)
    | OpcodeType.OP_SIZE -> Ok(Token.Size)
    | OpcodeType.OP_SWAP -> Ok(Token.Swap)
    | OpcodeType.OP_TUCK -> Ok(Token.Tuck)
    | OpcodeType.OP_VERIFY -> Ok(Token.Verify)
    | OpcodeType.OP_HASH160 -> Ok(Token.Hash160)
    | OpcodeType.OP_SHA256 -> Ok(Token.Sha256)
    | OpcodeType.OP_0 -> Ok(Token.Number 0u)
    | OpcodeType.OP_1 -> Ok(Token.Number 1u)
    | OpcodeType.OP_2 -> Ok(Token.Number 2u)
    | OpcodeType.OP_3 -> Ok(Token.Number 3u)
    | OpcodeType.OP_4 -> Ok(Token.Number 4u)
    | OpcodeType.OP_5 -> Ok(Token.Number 5u)
    | OpcodeType.OP_6 -> Ok(Token.Number 6u)
    | OpcodeType.OP_7 -> Ok(Token.Number 7u)
    | OpcodeType.OP_8 -> Ok(Token.Number 8u)
    | OpcodeType.OP_9 -> Ok(Token.Number 9u)
    | OpcodeType.OP_10 -> Ok(Token.Number 10u)
    | OpcodeType.OP_11 -> Ok(Token.Number 11u)
    | OpcodeType.OP_12 -> Ok(Token.Number 12u)
    | OpcodeType.OP_13 -> Ok(Token.Number 13u)
    | OpcodeType.OP_14 -> Ok(Token.Number 14u)
    | OpcodeType.OP_15 -> Ok(Token.Number 15u)
    | OpcodeType.OP_16 -> Ok(Token.Number 16u)
    | otherOp when (byte 0x01) < (byte otherOp) && (byte otherOp) < (byte 0x4B) -> 
        tryGetItemFromOp op
    | otherOp when (byte 0x4B) <= (byte otherOp) -> 
        Error(ParseException(sprintf "MiniScript does not support pushdata bigger than 33. Got %s" (otherOp.ToString())))
    | unknown ->
        Error(ParseException(sprintf "Unknown Opcode to MiniScript %s" (unknown.ToString())))

let private resultFolder (acc : Result<'a seq, ParseException>) 
    (item : Result<'a, 'c>) =
    match acc, item with
    | Ok x, Ok y -> 
        Ok(seq { 
               yield! x
               yield y
           })
    | Error x, Ok y -> Error x
    | Ok x, Error y -> Error y
    | Error x, Error y -> Error(ParseException((y.ToString()), x))

// Script -> Token list
let tokenize (script : Script) : Result<Token list, ParseException> =
    let ops = script.ToOps() |> Seq.map castOpToToken
    (Ok Seq.empty, ops)
    ||> Seq.fold resultFolder
    |> Result.map (fun ts -> Seq.toList ts) // we have to collect the elements since parser requires


type State = {
    ops: Op[]
    position: int
}

type TokenParser = Parser<AST> 

let nextToken state =
    if state.ops.Length - 1 < state.position then
        state, None
    else
        let newState = { state with position = state.position + 1 }
        let tk = state.ops.[state.position]
        newState, Some(tk)


module TokenParser =
    let pToken (cat: TokenCategory) =
        let name = sprintf "pToken %A" cat
        let innerFn state =
            if state.position < 0 then
                Error(name, "no more input", 0)
            else
                let pos = state.position
                let ops = state.ops.[pos]
                printfn "DEBUG: going to parse %s in position %d by parser (%s)" (ops.ToString()) (pos) name
                let r = castOpToToken ops
                match r with
                | Error pex ->
                    let msg = sprintf "opcode %s is not supported by MiniScript %s" ops.Name pex.Message
                    Error(name, msg, pos)
                | Ok actualToken ->
                    let actualCat = actualToken.GetCategory()
                    printfn "DEBUG: cat is %A\nactualCat is %A\n" cat actualCat
                    if cat = Any || cat = actualCat then
                        let newState = { state with position=state.position - 1 }
                        Ok (actualToken.GetItem(), newState) 
                    else
                        let msg = sprintf "token is not the one expected \nactual: %A\nexpected: %A" actualCat cat
                        Error(name, msg, pos)
        {parseFn=innerFn; name=name}

    let mutable pW, pWImpl = createParserForwardedToRef<AST, State>()
    let mutable pE, pEImpl = createParserForwardedToRef<AST, State>()
    let mutable pV, pVImpl = createParserForwardedToRef<AST, State>()
    let mutable pQ, pQImpl = createParserForwardedToRef<AST, State>()

    // ---- E ---------
    let pEParallelAnd = ((pToken BoolAnd)
                        >>. pW .>>. pE
                        |>> fun (astW, astE) ->
                            ETree(E.ParallelAnd(astE.castEUnsafe(), astW.castWUnsafe())))
                         <?> "Parser E.ParallelAnd"

    let pECheckSig = (pToken CheckSig)
                     >>. (pToken Pk)
                     |>> fun maybePKObj -> ETree(E.CheckSig (maybePKObj.Value :?> NBitcoin.PubKey))
                     <?> "Parser E.Checksig"

    // ---- W ---------
    let pWCheckSig = (pToken CheckSig)
                     >>. (pToken Pk) .>> (pToken Swap)
                     |>> fun maybePKObj -> WTree(W.CheckSig (maybePKObj.Value :?> NBitcoin.PubKey))
                     <?> "Parser W.Checksig"

    let pTime1 = (pToken EndIf)
                 >>. (pToken (Drop))
                 >>. (pToken (CheckSequenceVerify))
                 >>. (pToken Number)
                 .>> (pToken If) .>> (pToken Dup)

    let pWTime = (pTime1
                 .>> (pToken Swap)
                 |>> fun o -> WTree(W.Time(o.Value :?> uint32)))
                 <?> "Parser pWTime"

    // ---- V -------
    let pVDelayedOr = ((pToken CheckSigVerify)
                      >>. (pToken EndIf) >>. pQ) .>>. (pToken Else >>. pQ .>> pToken If)
                      |>> fun (q1, q2) -> VTree(V.DelayedOr(q2.castQUnsafe(), q1.castQUnsafe()))

    // ---- Q -------
    let pQPubKey = (pToken Pk)
                   |>> fun pk -> QTree(Q.Pubkey(pk.Value :?> NBitcoin.PubKey))
    // ---- T -------
    // ---- F -------
    do pWImpl := (choice [pWCheckSig; pWTime])
    do pEImpl := (choice [pECheckSig; pEParallelAnd])
    do pVImpl := choice [pVDelayedOr]
    do pQImpl := choice [pQPubKey]

    let ASTParser = choice [pW; pE; pQ; pV]

let run parser (inputScript: Script) =
    let ops = (inputScript.ToOps() |> Seq.toArray)
    parser.parseFn {ops=ops; position=ops.Length - 1}

let parseScript (sc: Script) =
    run TokenParser.ASTParser sc

