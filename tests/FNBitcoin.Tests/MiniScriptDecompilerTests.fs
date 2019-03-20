module MiniScriptDecompilerTests

open Expecto
open Expecto.Logging
open NBitcoin
open FNBitcoin.Tests.Generators
open FNBitcoin.MiniScriptAST
open FNBitcoin.MiniScript
open FNBitcoin.MiniScriptDecompiler

let logger = Log.create "MiniscriptDeCompiler"
let keys =
    [ "028c28a97bf8298bc0d23d8c749452a32e694b65e30a9472a3954ab30fe5324caa"; 
      "03ab1ac1872a38a2f196bed5a6047f0da2c8130fe8de49fc4d5dfb201f7611d8e2"; 
      "039729247032c0dfcf45b4841fcd72f6e9a2422631fc3466cf863e87154754dd40"; 
      "032564fe9b5beef82d3703a607253f31ef8ea1b365772df434226aee642651b3fa"; 
      "0289637f97580a796e050791ad5a2f27af1803645d95df021a3c2d82eb8c2ca7ff" ]

let checkParseResult res expected =
    match res with
    | Ok (ast) -> Expect.equal ast expected "failed to deserialize properly"
    | Result.Error e ->
    let name, msg, pos = e
    failwithf "name: %s\nmsg: %s\npos: %d" name msg pos

[<Tests>]
let tests =
    testList "Decompiler" [ testCase "case1" <| fun _ ->
                                let pk = PubKey(keys.[0])
                                let pk2 = PubKey(keys.[1])
                                let boolAndWE = ETree(
                                    E.ParallelAnd(
                                        E.CheckSig(pk), W.Time(1u))
                                    )
                                let sc = boolAndWE.ToScript()
                                printfn "going to parse %A"  sc
                                let res = FNBitcoin.MiniScriptDecompiler.parseScript sc
                                checkParseResult res boolAndWE

                            testCase "case2" <| fun _ ->

                                let pk = PubKey(keys.[0])
                                let pk2 = PubKey(keys.[1])
                                let delayedOrV = VTree(V.DelayedOr(Q.Pubkey(pk), Q.Pubkey(pk2)))
                                let sc = delayedOrV.ToScript()
                                printfn "going to parse %A" sc
                                let res = FNBitcoin.MiniScriptDecompiler.parseScript sc
                                checkParseResult res delayedOrV

                            ftestCase "Should pass the testcase in rust-miniscript" <| fun _ -> 
                               let keysList =
                                   keys
                                   |> List.map (PubKey)
                                   |> List.toArray

                               let roundtrip (miniscriptResult : Result<MiniScript, string>) 
                                   (s : Script) =
                                   match miniscriptResult with
                                   | Ok tree -> 
                                       let ser = tree.ToScript()
                                       Expect.equal ser s 
                                           "Serialized Miniscript does not match expected script"
                                       let deser =
                                           MiniScript.fromScriptUnsafe s
                                       Expect.equal deser tree 
                                           "deserialized script does not match expected MiniScript"
                                   | Result.Error e -> failwith e

                               let r1 =
                                   MiniScript.fromAST 
                                       (AST.TTree
                                            (T.CastE
                                                 (E.CheckSig
                                                      (PubKey
                                                           (keys.[0])))))
                               let s1 =
                                   Script
                                       (sprintf "%s %s" 
                                            (keys.[0].ToString()) 
                                            "OP_CHECKSIG")
                               roundtrip r1 s1
                               let r2 =
                                   MiniScript.fromAST 
                                       (AST.TTree
                                            (T.CastE
                                                 (E.CheckMultiSig
                                                      (3u, keysList))))
                               let s2 =
                                   Script
                                       (sprintf 
                                            "OP_3 %s %s %s %s %s OP_5 OP_CHECKMULTISIG" 
                                            keys.[0] keys.[1] 
                                            keys.[2] keys.[3] 
                                            keys.[4])
                               roundtrip r2 s2

                               let r3_partial =
                                   MiniScript.fromAST(TTree(T.And
                                      (V.CheckMultiSig
                                          (2u, 
                                           keysList.[2..3]), 
                                       T.Time(10000u))
                                      ))

                               let policy3_2 =
                                   sprintf 
                                       "2 %s %s 2 OP_CHECKMULTISIGVERIFY" 
                                       keys.[2] keys.[3]

                               let s3_partial = Script(sprintf "%s 2710 OP_CSV" policy3_2)
                               roundtrip r3_partial s3_partial

                               // Liquid policy
                               let r3 =
                                   MiniScript.fromAST 
                                       (AST.TTree
                                            (T.CascadeOr
                                                 (E.CheckMultiSig
                                                      (2u, 
                                                       keysList.[0..1]), 
                                                  T.And
                                                      (V.CheckMultiSig
                                                           (2u, 
                                                            keysList.[2..3]), 
                                                       T.Time
                                                           (10000u)))))
                               let policy3_1 =
                                   sprintf 
                                       "2 %s %s 2 OP_CHECKMULTISIG" 
                                       keys.[0] keys.[1]
                               let tmp = sprintf "%s OP_IFDUP OP_NOTIF %s 2710 OP_CSV OP_ENDIF"
                                                 policy3_1 policy3_2
                               let s3 =
                                   Script(tmp)
                               roundtrip r3 s3
                               let r4 =
                                   MiniScript.fromAST 
                                       (TTree(T.Time(921u)))
                               let s4 = Script("0399 OP_CSV")
                               roundtrip r4 s4
                           ]