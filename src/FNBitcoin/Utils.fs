module FNBitcoin.Utils

open System.Collections.Generic
(*
type Utils =
    let dict = Dictionary<_, _>()
    let memoize f =
        let memoizedF n =
            match dict.TryGetValue(n) with
            | (true, v) -> v
            | (false, _) -> 
                let tmp = f n
                dict.Add(n, tmp)
                tmp
        memoizedF

*)


let inline (!>) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b )x )