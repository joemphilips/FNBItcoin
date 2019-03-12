module FNBitcoin.MiniScriptDecompiler
open NBitcoin
open System
open System.Collections.Generic

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

type ParseException(msg, ex: exn) =
    inherit Exception(msg, ex)
    new (msg) = ParseException(msg, null)

let private castOpToToken (op: Op): Result<Token, ParseException> =
    match (op.Code) with
    | OpcodeType.OP_BOOLAND -> Ok(Token.BoolAnd)
    | OpcodeType.OP_BOOLOR -> Ok(Token.BoolOr)
    | OpcodeType.OP_EQUAL -> Ok(Token.Equal)
    | OpcodeType.OP_EQUALVERIFY -> Ok(Token.EqualVerify)
    | OpcodeType.OP_CHECKSIG -> Ok(Token.CheckSig)
    | OpcodeType.OP_CHECKSIGVERIFY -> Ok(Token.CheckSigVerify)
    | OpcodeType.OP_CHECKMULTISIG -> Ok(Token.CheckMultiSig)
    | OpcodeType.OP_CHECKMULTISIGVERIFY -> Ok(Token.CheckMultiSigVerify)
    | OpcodeType.OP_CHECKSEQUENCEVERIFY -> Ok(Token.CheckMultiSigVerify)
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
    | OpcodeType.OP_PUSHDATA1
    | OpcodeType.OP_PUSHDATA2
    | OpcodeType.OP_PUSHDATA4 ->
        let size = op.PushData.Length
        match size with
        | 20 -> Ok(Token.Hash160Hash (uint160(op.PushData)))
        | 32 -> Ok(Token.Sha256Hash (uint256(op.PushData)))
        | 33 ->
            try 
                Ok(Pk(NBitcoin.PubKey(op.PushData)))
            with
            | :? FormatException as ex -> Error(ParseException("Invalid Public Key", ex))
        | _ ->
            match op.GetInt().HasValue with
            | true ->
                let v = op.GetInt().Value
                /// no need to check v >= 0 since it is checked in NBitcoin side
                Ok(Token.Number(uint32 v))
            | false ->
                Error(ParseException(sprintf "Invalid push for %O" op))
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
    | unknown -> Error(ParseException(sprintf "Unknown Opcode %s" (unknown.ToString())))

let private resultFolder (acc: Result<'a seq, ParseException>) (item: Result<'a, 'c>) =
    match acc, item with
    | Ok x, Ok y -> Ok( seq {yield! x; yield y})
    | Error x, Ok y -> Error x
    | Ok x, Error y -> Error y
    | Error x, Error y -> Error (ParseException((y.ToString()), x))

let tokenize(script: Script): Result<Token seq, ParseException> =
    let ops = script.ToOps() |> Seq.map castOpToToken
    (Ok Seq.empty, ops) ||> Seq.fold resultFolder