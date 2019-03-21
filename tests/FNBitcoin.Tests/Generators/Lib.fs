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
                        let shrinkThres (k, (ps: Policy[])) =
                            let k2 = if k = 1u then k else k - 1u
                            let ps2 = ps.[0..(ps.Length - 2)]
                            [Policy.Threshold(k2, ps2)]
                        let subexpr = ps |> Array.toList
                        if ps.Length = 1 then subexpr else shrinkThres(k, ps)
                    | Policy.And(p1, p2) -> [p1; p2]
                    | Policy.Or(p1, p2) -> [p1; p2]
                    | Policy.AsymmetricOr(p1, p2) -> [p1; p2]

                shrinkPolicy p |> List.toSeq
        }
