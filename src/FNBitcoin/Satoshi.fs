module FNBitcoin.Satoshi

[<Measure>]
type satoshi

[<Measure>]
type btc

let MAX_MONEY = 21_000_000I

type Money = private MoneyImpl of bigint

module Money =
    let fromBTC n : Money =
        if MAX_MONEY < n then 
            invalidArg (sprintf "%A" n) "money should be less then %A BTC"
        else MoneyImpl n
    
    let (|Money|) (MoneyImpl n) = n
