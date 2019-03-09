namespace FNBitcoin.Tests.Generators
open FsCheck
open FNBitcoin.MiniScriptParser
open FNBitcoin.Tests.Generators.Policy

type Generators =
    static member Policy() =
        { new Arbitrary<Policy>() with
            override x.Generator = policy
            override x.Shrinker t = Seq.empty
        }