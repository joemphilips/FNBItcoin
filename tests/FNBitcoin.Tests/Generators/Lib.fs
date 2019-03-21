namespace FNBitcoin.Tests.Generators

open FsCheck
open FNBitcoin.MiniScriptParser
open FNBitcoin.Tests.Generators.Policy

type Generators =
    static member Policy() : Arbitrary<Policy> =  // policy |> Arb.fromGen
        { new Arbitrary<Policy>() with
            override this.Generator = policy
            // TODO: This shrinker is far from ideal
            override this.Shrinker(p: Policy) =
                let shrinkPolicy p =
                    match p with
                    | Key k -> []
                    | Multi(m, pks) -> [Multi(1u, pks.[0..0]); Multi(1u, pks.[pks.Length-2..pks.Length-1]); Policy.Key pks.[0]]
                    | Policy.Hash h -> []
                    | Policy.Time t -> []
                    | Policy.Threshold (k, ps) ->
                        ps |> Array.toList
                    | Policy.And(p1, p2) -> [p1; p2]
                    | Policy.Or(p1, p2) -> [p1; p2]
                    | Policy.AsymmetricOr(p1, p2) -> [p1; p2]

                shrinkPolicy p |> List.toSeq
        }
