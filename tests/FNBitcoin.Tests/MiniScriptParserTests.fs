module MiniScriptParserTests

open FNBitcoin.MiniScriptParser
open Expecto

[<Tests>]
let tests =
    testList "miniscript parser tests" [
        testCase "Should print" <| fun _ ->
            let pk1Str = "0225629523a16986d3bf575e1163f7dff024d734ad1e656b55ba94e41ede2fdfb6"
            let pk2Str = "03b0ad2ab4133717f26f3a50dfb3a0df0664c88045a1b80005aac88284003a98d3"
            let pk3Str = "02717a8c5d9fc77bc12cfe1171f51c5e5178048a5b8f66ca11088dced56d8bf469"
            let pk1, pk2, pk3 = NBitcoin.PubKey(pk1Str), NBitcoin.PubKey(pk2Str), NBitcoin.PubKey(pk3Str)
            let testdata1: Policy<_> = And(Key(pk1), Or(Multi(1u, [|pk2; pk3|]), AsymmetricOr(Key(pk1), Time(1000u))))
            let actual = testdata1.print()
            let expected = sprintf "and(pk(%s),or(multi(1,%s,%s),aor(pk(%s),time(1000))))" pk1Str pk2Str pk3Str pk1Str
            Expect.equal actual expected "Policy.print did not work as expected"
        testCase "ParseTest1" <| fun _ ->
            let data1 = "pk(0225629523a16986d3bf575e1163f7dff024d734ad1e656b55ba94e41ede2fdfb6)"
            match data1 with
            | Policy p -> printfn "%A" p
            | _ -> failwith "Failed to parse policy"

        testCase "ParseTest2" <| fun _ ->
            let data =  "thres(2, pk())"
    ]
