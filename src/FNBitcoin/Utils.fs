module FNBitcoin.Utils
open System.Collections.Generic

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