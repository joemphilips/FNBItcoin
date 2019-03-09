namespace FNBitcoin.Tests.Generators

module internal NBitcoin =
    open FsCheck
    open NBitcoin
    open FNBitcoin.Tests.Generators.Primitives

    let pubKeyGen =
        Gen.constant(Key()) |> Gen.map(fun k -> k.PubKey)

    let uint256Gen =
        bytesOfNGen 32 |> Gen.map uint256
