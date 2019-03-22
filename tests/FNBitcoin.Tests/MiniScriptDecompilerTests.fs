module MiniScriptDecompilerTests

open Expecto
open Expecto.Logging
open NBitcoin
open FNBitcoin.MiniScriptParser
open FNBitcoin.Tests.Generators
open FNBitcoin.MiniScriptAST
open FNBitcoin.MiniScript
open FNBitcoin.Utils.Parser
open FNBitcoin.MiniScriptCompiler
open FNBitcoin.MiniScriptDecompiler

let logger = Log.create "MiniscriptDeCompiler"
let keys =
    [ "028c28a97bf8298bc0d23d8c749452a32e694b65e30a9472a3954ab30fe5324caa"; 
      "03ab1ac1872a38a2f196bed5a6047f0da2c8130fe8de49fc4d5dfb201f7611d8e2"; 
      "039729247032c0dfcf45b4841fcd72f6e9a2422631fc3466cf863e87154754dd40"; 
      "032564fe9b5beef82d3703a607253f31ef8ea1b365772df434226aee642651b3fa"; 
      "0289637f97580a796e050791ad5a2f27af1803645d95df021a3c2d82eb8c2ca7ff" ]

let keysList =
   keys
   |> List.map (PubKey)
   |> List.toArray

let longKeysList = keysList.[0] |> Array.replicate 20
// --------- AST <-> Script ---------
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

                            testCase "Should pass the testcase in rust-miniscript" <| fun _ -> 

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

                               let s3_partial = Script(sprintf "%s 1027 OP_CSV" policy3_2)
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
                               let tmp = sprintf "%s OP_IFDUP OP_NOTIF %s 1027 OP_CSV OP_ENDIF"
                                                 policy3_1 policy3_2
                               let s3 =
                                   Script(tmp)
                               roundtrip r3 s3

                               let r4 =
                                   MiniScript.fromAST 
                                       (TTree(T.Time(921u)))
                               let s4 = Script("9903 OP_CSV")
                               roundtrip r4 s4

                               let r5 = MiniScript.fromAST (TTree(
                                    T.SwitchOrV(
                                        V.CheckSig(keysList.[0]),
                                        V.And(
                                            V.CheckSig(keysList.[1]),
                                            V.CheckSig(keysList.[2])
                                        )
                                    )))

                               let scriptStr = sprintf "OP_IF %s OP_CHECKSIGVERIFY OP_ELSE %s OP_CHECKSIGVERIFY %s OP_CHECKSIGVERIFY OP_ENDIF 1"
                                                       keys.[0] keys.[1] keys.[2]
                               let s5 = Script(scriptStr)
                               roundtrip r5 s5

                           ]

// --------- converting all the way down to ----
// --------- Policy <-> AST <-> Script ---------
let config =
    { FsCheckConfig.defaultConfig with arbitrary = [ typeof<Generators> ]
                                       maxTest = 30
                                       endSize = 32 }

let roundTripFromMiniScript (m: MiniScript) =
    let sc = m.ToScript()
    printfn "going to decompile %A: It should be %A" sc m
    let m2 = MiniScript.fromScriptUnsafe sc
    Expect.equal m2 m "failed"

let roundtrip p =
    let m = CompiledNode.fromPolicy(p).compileUnsafe()
    roundTripFromMiniScript m

let hash = uint256.Parse("59141e52303a755307114c2a5e6823010b3f1d586216742f396d4b06106e222c")
[<Tests>]
let tests2 =
    testList "Should convert Policy <-> AST <-> Script" [
        ftestPropertyWithConfig config "Every possible MiniScript"  <| fun (p: Policy) ->
            roundtrip p
        testCase "Case found by property tests: 1" <| fun _ ->
            let input = Policy.Or(Key(keysList.[0]), Policy.And(Policy.Time(2u), Policy.Time(1u)))
            let m = CompiledNode.fromPolicy(input).compileUnsafe()
            let sc = m.ToScript()
            printfn "going to decompile %A: It should be %A" sc m
            let customParser = TokenParser.pT
            let ops = sc.ToOps() |> Seq.toArray
            let customState = {ops=ops; position=ops.Length - 1}
            let m2 = run customParser customState
            Expect.isOk m2 "failed"
        testCase "Case found by property tests: 2" <| fun _ ->
            let input = MiniScript.fromASTUnsafe(TTree(T.HashEqual(hash)))
            roundTripFromMiniScript input
        testCase "Case found by property tests: 3" <| fun _ ->
            let input = MiniScript.fromASTUnsafe(
                TTree(T.And(V.Time(1u), T.Time(1u))))
            roundTripFromMiniScript input

        testCase "Case found by property tests: 4" <| fun _ ->
            let input = MiniScript.fromASTUnsafe(TTree(
                T.CastE(
                    E.Threshold(
                            1u,
                            E.CheckSig(keysList.[0]),
                            [| W.CheckSig(keysList.[0]) |]
                            )
                        )
                    )
                )
            roundTripFromMiniScript input
        testCase "Case found by property tests: 5" <| fun _ ->
            let input = MiniScript.fromASTUnsafe(TTree(
                T.CastE(
                    E.Likely(
                        F.Threshold(
                            1u,
                            E.CheckSig(keysList.[0]),
                            [| W.CheckSig(keysList.[0]) |]
                            )
                        )
                    ))
                )
            roundTripFromMiniScript input
        testCase "Case found by property tests: 6" <| fun _ ->
            let input = MiniScript.fromASTUnsafe(TTree(
                T.And(
                    V.CheckMultiSig(1u, longKeysList),
                    T.Time(1u)
                )))
            roundTripFromMiniScript input
    ]

let roundtripParserAndAST (parser: Parser<_, _>) (ast: AST) =
    let sc = ast.ToScript()
    printfn "going to parse script %A\nit should be AST %A" sc ast
    let ops = sc.ToOps() |> Seq.toArray
    let initialState = {ops=ops;position=ops.Length - 1}
    match run parser initialState with
    | Ok r -> Expect.equal ast (fst r) "AST is not equal"
    | Result.Error e -> failwithf "%A" e

[<Tests>]
let deserializationTestWithParser =
    testList "deserialization test with parser" [
        testCase "Case found by property tests: 5_2" <| fun _ ->
            let input = 
                    ETree(
                        E.Likely(
                            F.Time(1u)
                    )
                )
            let parser = TokenParser.pE
            roundtripParserAndAST parser input
        testCase "Case found by property tests: 5_3" <| fun _ ->
            let input = 
                        FTree(F.Threshold(
                            1u,
                            E.CheckSig(keysList.[0]),
                            [| W.CheckSig(keysList.[0]) |]
                            ))
            let parser = TokenParser.pF
            roundtripParserAndAST parser input
        testCase "Case found by property tests: 6_1" <| fun _ ->
            let input = 
                VTree(V.CheckMultiSig(1u, longKeysList))
            let parser = TokenParser.pV
            roundtripParserAndAST parser input
        ftestCase "Case found by property tests: 7_1" <| fun _ ->
            let input = 
                WTree(W.CastE(E.CheckSig(keysList.[0])))
            let parser = TokenParser.pW
            roundtripParserAndAST parser input

        testCase "Case found by property tests: 7_2" <| fun _ ->
            let input =
                WTree(W.CastE(E.SwitchOrRight(E.Time(1u), F.Time(1u))))

            let parser = TokenParser.pW
            roundtripParserAndAST parser input
        testCase "Case found by property tests: 8_1" <| fun _ ->
            let input =
                FTree(F.SwitchOr(F.Time(1u), F.Time(1u)))

            let parser = TokenParser.pF
            roundtripParserAndAST parser input
        testCase "Case found by property tests: 8_2" <| fun _ ->
            let input =
                VTree(V.SwitchOr(V.Time(1u), V.Time(1u)))

            let parser = TokenParser.pV
            roundtripParserAndAST parser input

            let input =
                TTree(T.SwitchOr(T.Time(1u), T.Time(1u)))
            let parser = TokenParser.pT
            roundtripParserAndAST parser input

            let input =
                VTree(V.SwitchOrT(T.Time(1u), T.Time(1u)))
            let parser = TokenParser.pV
            roundtripParserAndAST parser input
    ]