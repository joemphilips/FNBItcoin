module MiniscriptAST
open NBitcoin

type E =
    | CheckSig of PubKey
    | CheckMultiSig of uint32 * PubKey []
    | Time of uint32
    | Threshold of (uint32 * E * W [])
    | ParallelAnd of (E * W)
    | CascadeAnd of (E * F)
    | ParallelOr of (E * W)
    | CascadeOr of (E * E)
    | SwitchOrLeft of (E * F)
    | SwitchOrRight of (E * F)
    | Likely of F
    | Unlikely of F
and Q =
    | Pubkey of PubKey
    | And of (V * Q)
    | Or of (Q * Q)
and W =
    | CheckSig of PubKey
    | HashEqual of uint256
    | Time of uint32
    | CastE of E
and F =
    | CheckSig of PubKey
    | CheckMultiSig of uint32 * PubKey []
    | Time of uint32
    | HashEqual of uint256
    | Threshold of (E * W list)
    | And of (V * F)
    | CascadeOr of (E * V)
    | SwitchOr of (F * F)
    | SwitchOrV of (V * V)
    | DelayedOr of (Q * Q)
and V =
    | CheckSig of PubKey
    | CheckMultiSig of uint32 * PubKey []
    | Time of uint32
    | HashEqual of uint256
    | Threshold of (E * W list)
    | And of (V * V)
    | CascadeOr of (E * V)
    | SwitchOr of (V * V)
    | SwitchOrT of (T * T)
    | DelayedOr of (Q * Q)
and T =
    | Time of uint32
    | HashEqual of uint256
    | And of (V * T)
    | ParallelOr of (E * W)
    | CascadeOr of (E * T)
    | CascadeOrV of (E * V)
    | SwitchOr of (T * T)
    | SwitchOrV of (V * V)
    | DelayedOr of (Q * Q)
    | CastE of E

type AST =
    | ETree of E
    | QTree of Q
    | WTree of W
    | FTree of F
    | VTree of V
    | TTree of T

type AST with
    member this.castT(): Result<T, string> =
        match this with
        | TTree t -> Ok t
        | _ -> Error "failed to cast"

    member this.castE(): Result<E, string> =
        match this with
        | ETree e -> Ok e
        | _ -> Error "failed to cast"

    member this.castQ(): Result<Q, string> =
        match this with
        | QTree q -> Ok q
        | _ -> Error "failed to cast"

    member this.castW(): Result<W, string> =
        match this with
        | WTree w -> Ok w
        | _ -> Error "failed to cast"

    member this.castF(): Result<F, string> =
        match this with
        | FTree f -> Ok f
        | _ -> Error "failed to cast"

    member this.castV(): Result<V, string> =
        match this with
        | VTree v -> Ok v
        | _ -> Error "failed to cast"

    member this.castTUnsafe(): T =
        match this.castT() with
        | Ok t -> t
        | Error s -> failwith s

    member this.castEUnsafe(): E =
        match this.castE() with
        | Ok e -> e
        | Error s -> failwith s

    member this.castQUnsafe(): Q =
        match this.castQ() with
        | Ok q -> q
        | Error s -> failwith s

    member this.castWUnsafe(): W =
        match this.castW() with
        | Ok w -> w
        | Error s -> failwith s

    member this.castFUnsafe(): F =
        match this.castF() with
        | Ok f -> f
        | Error s -> failwith s

    member this.castVUnsafe(): V =
        match this.castV() with
        | Ok v -> v
        | Error s -> failwith s