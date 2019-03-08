module FNBitcoin.Contract
open NBitcoin
open System

type Verifiable<'T> =
    | Verified of 'T
    | Failed of string

module Verifiable =
    let mapV v (f: 'a -> 'b) =
        match v with
        | Verified (item) -> Verified (f item)
        | Failed reason -> Failed reason

    let applyV v fV =
        match v fV with
        | Verified innerV, Verified innerFV ->  Verified(innerFV innerV)
        | Failed s1, Failed s2 -> Failed (s1 + s2)
        | _, Failed s -> Failed s
        | Failed s, _ -> Failed s

    let (<!>) = mapV
    let (<*>) = applyV

    let bind x f =
        match x with
        | Verified item -> f (item)
        | Failed s -> Failed s

    let (>>=) = bind

    type VerifiableBuilder() =
        member this.Bind(x, f) = bind x f
        member this.Return(x) = Verified x
        member this.ReturnFrom(x) = x
        member this.Zero() = Verified ()

let verify = Verifiable.VerifiableBuilder()

type Contract<'In, 'Out> = 'In -> Verifiable<'Out>

module Contract =
    let Multisig (threshold: int, pubKeys: PubKey[]) =
        Verified(PayToMultiSigTemplate.Instance.GenerateScriptPubKey(threshold, pubKeys))

    let TimeOut (pubkey: PubKey, time: DateTimeOffset)
                (amount: Money) =
        let scriptStr = sprintf "%d OP_CHECKSEQUENCEVERIFY OP_DROP %s OP_DROP" (time.ToUnixTimeSeconds()) (pubkey.ToHex())
        Verified(Script(scriptStr))

    let EscrowWithDelay (sender: PubKey, recipient: PubKey, escrow: PubKey, delay: DateTimeOffset)
                        (amount: Money) =
        verify {
            let! multiSigScript = Multisig (2, [| sender; recipient; escrow |])
            let! timeoutScript = TimeOut (sender, delay) (amount)
            return [|multiSigScript, timeoutScript|]
        }

    let SpendEscrowWithDelay (source: Transaction,
                              scripts: Script array,
                              output: BitcoinAddress,
                              amount: Money) =
        let txb = source.GetConsensusFactory().CreateTransactionBuilder()
        txb.AddKnownRedeems(scripts) |> ignore
        txb.AddCoins(source.Outputs.AsCoins() |> Seq.map(fun c -> c :> ICoin)) |> ignore
        txb.Send(output, amount)
