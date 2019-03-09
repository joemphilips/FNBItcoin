module MiniScriptParserTests

open FNBitcoin.MiniScriptParser
open Expecto
open Expecto.Logging
open FNBitcoin.Tests.Generators
let logger = Log.create "MiniscriptParser"

let pk1Str = "0225629523a16986d3bf575e1163f7dff024d734ad1e656b55ba94e41ede2fdfb6"
let pk2Str = "03b0ad2ab4133717f26f3a50dfb3a0df0664c88045a1b80005aac88284003a98d3"
let pk3Str = "02717a8c5d9fc77bc12cfe1171f51c5e5178048a5b8f66ca11088dced56d8bf469"

let check = function
            | Policy p -> ()
            | _ -> failwith "Failed to parse policy"


let config = {
            FsCheckConfig.defaultConfig with arbitrary = [typeof<Generators>]
        }

[<Tests>]
let tests =
    testList "miniscript parser tests" [
        ptestCase "Should print" <| fun _ ->
            let pk1, pk2, pk3 = NBitcoin.PubKey(pk1Str), NBitcoin.PubKey(pk2Str), NBitcoin.PubKey(pk3Str)
            let testdata1: Policy = And(Key(pk1), Or(Multi(1u, [|pk2; pk3|]), AsymmetricOr(Key(pk1), Time(1000u))))
            let actual = testdata1.print()
            let expected = sprintf "and(pk(%s),or(multi(1,%s,%s),aor(pk(%s),time(1000))))" pk1Str pk2Str pk3Str pk1Str
            Expect.equal actual expected "Policy.print did not work as expected"
        ptestCase "ParseTest1" <| fun _ ->
            let data1 = "pk(0225629523a16986d3bf575e1163f7dff024d734ad1e656b55ba94e41ede2fdfb6)"
            check data1

        ptestCase "parsing input with noise" <| fun _ ->
            let dataWithWhiteSpace =  sprintf "thres ( 2 , and (pk ( %s ) , aor( multi ( %s , %s ) , time  (  1000)) ))"
                                        pk1Str pk1Str pk1Str
            check dataWithWhiteSpace
            let dataWithNewLine =  sprintf "thres ( \r\n2 , and \n(pk ( \n%s ) , aor( multi \n( %s ,%s )\n, time  (  1000)) ))"
                                      pk1Str pk1Str pk1Str
            check dataWithNewLine

        ptestCase "symmetrical conversion" <| fun _ ->
            let data =  sprintf "thres(2,and(pk(%s),aor(multi(%s,%s),time(1000))))" pk1Str pk1Str pk1Str
            let data2 =  match data with
                         | Policy p -> printPolicy p
                         | _ -> failwith "Failed to parse policy"
            Expect.equal data data2 "Could not parse symmetrically"

        testPropertyWithConfig config "Serialization should be bidirectional" <| fun p ->
                ()
    ]

