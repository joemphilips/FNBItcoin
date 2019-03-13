namespace FNBitcoin.Tests.Generators

module internal NBitcoin =
    open FsCheck
    open NBitcoin
    open FNBitcoin.Tests.Generators.Primitives
    
    let pubKeyGen =
        let k = Key() // prioritize speed for randomness
        Gen.constant (k) |> Gen.map (fun k -> k.PubKey)
    
    let uint256Gen = bytesOfNGen 32 |> Gen.map uint256
