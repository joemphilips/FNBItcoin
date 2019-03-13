namespace FNBitcoin.Tests.Generators

open FsCheck
open FNBitcoin.MiniScriptParser
open FNBitcoin.Tests.Generators.Policy

type Generators =
    static member Policy() : Arbitrary<Policy> = policy |> Arb.fromGen
