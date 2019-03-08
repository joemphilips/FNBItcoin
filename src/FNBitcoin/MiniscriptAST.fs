module internal MiniscriptAST
open NBitcoin

type E<'P> =
    | CheckSig of 'P
    | CheckMultiSig of 'P list
    | Time of uint32
    | Threshold of (E<'P> * W<'P> list)
    | ParallelAnd of (E<'P> * W<'P>)
    | CascadeAnd of (E<'P> * F<'P>)
    | ParallelOr of (E<'P> * W<'P>)
    | CascadeOr of (E<'P> * E<'P>)
    | SwitchOrLeft of (E<'P> * F<'P>)
    | SwitchOrRight of (E<'P> * F<'P>)
    | Likely of F<'P>
    | Unlikely of F<'P>
and Q<'P> =
    | Pubkey of 'P
    | And of (V<'P> * Q<'P>)
    | Or of (Q<'P> * Q<'P>)
and W<'P> =
    | CheckSig of 'P
    | HashEqual of uint256
    | Time of uint32
    | CastE of E<'P>
and F<'P> =
    | CheckSig of 'P
    | CheckMultiSig of 'P list
    | Time of uint32
    | HashEqual of uint256
    | Threshold of (E<'P> * W<'P> list)
    | And of (V<'P> * F<'P>)
    | CascadeOr of (E<'P> * V<'P>)
    | SwitchOr of (F<'P> * F<'P>)
    | SwitchOrV of (V<'P> * V<'P>)
    | DelayedOr of (Q<'P> * Q<'P>)
and V<'P> =
    | CheckSig of 'P
    | CheckMultiSig of 'P list
    | Time of uint32
    | HashEqual of uint256
    | Threshold of (E<'P> * W<'P> list)
    | And of (V<'P> * V<'P>)
    | CascadeOr of (E<'P> * V<'P>)
    | SwitchOr of (V<'P> * V<'P>)
    | SwitchOrT of (T<'P> * T<'P>)
    | DelayedOr of (Q<'P> * Q<'P>)
and T<'P> =
    | Time of uint32
    | HashEqual of uint256
    | And of (V<'P> * T<'P>)
    | CascadeOr of (E<'P> * T<'P>)
    | CascadeOrV of (E<'P> * V<'P>)
    | SwitchOr of (T<'P> * T<'P>)
    | SwitchOrV of (V<'P> * V<'P>)
    | DelayedOr of (Q<'P> * Q<'P>)
    | CastE of E<'P>
