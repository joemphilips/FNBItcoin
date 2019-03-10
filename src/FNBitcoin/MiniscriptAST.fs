module internal MiniscriptAST
open NBitcoin

type E =
    | CheckSig of PubKey
    | CheckMultiSig of PubKey list
    | Time of uint32
    | Threshold of (E * W list)
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
    | CheckMultiSig of PubKey list
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
    | CheckMultiSig of PubKey list
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
    | CascadeOr of (E * T)
    | CascadeOrV of (E * V)
    | SwitchOr of (T * T)
    | SwitchOrV of (V * V)
    | DelayedOr of (Q * Q)
    | CastE of E

type AST =
    | E
    | Q
    | W
    | F
    | V
    | T

