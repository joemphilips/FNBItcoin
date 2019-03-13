module FNBitcoin.MiniScriptAST

open NBitcoin

// TODO: Use unativeint instead of uint
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
    | Threshold of (uint32 * E * W [])
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
    | Threshold of (uint32 * E * W [])
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

type E with
    member this.print() =
        match this with
        | CheckSig pk -> sprintf "E.pk(%s)" (pk.ToHex())
        | CheckMultiSig(m, pks) -> 
            sprintf "E.multi(%d,%s)" m 
                (pks 
                 |> Array.fold (fun acc k -> sprintf "%s,%s" acc (k.ToString())) 
                        "")
        | Time t -> sprintf "E.time(%d)" t
        | Threshold(num, e, ws) -> 
            sprintf "E.thres(%d,%s,%s)" num (e.print()) 
                (ws 
                 |> Array.fold (fun acc w -> sprintf "%s,%s" acc (w.print())) "")
        | ParallelAnd(e, w) -> sprintf "E.and_p(%s,%s)" (e.print()) (w.print())
        | CascadeAnd(e, f) -> sprintf "E.and_c(%s,%s)" (e.print()) (f.print())
        | ParallelOr(e, w) -> sprintf "E.or_p(%s,%s)" (e.print()) (w.print())
        | CascadeOr(e, e2) -> sprintf "E.or_c(%s,%s)" (e.print()) (e2.print())
        | SwitchOrLeft(e, f) -> sprintf "E.or_s(%s,%s)" (e.print()) (f.print())
        | SwitchOrRight(e, f) -> sprintf "E.or_r(%s,%s)" (e.print()) (f.print())
        | Likely f -> sprintf "E.lift_l(%s)" (f.print())
        | Unlikely f -> sprintf "E.lift_u(%s)" (f.print())

and Q with
    member this.print() =
        match this with
        | Pubkey p -> sprintf "Q.pk(%s)" (p.ToString())
        | And(v, q) -> sprintf "Q.and(%s,%s)" (v.print()) (q.print())
        | Or(q1, q2) -> sprintf "Q.or(%s,%s)" (q1.print()) (q2.print())

and W with
    member this.print() =
        match this with
        | CheckSig pk -> sprintf "W.pk(%s)" (pk.ToString())
        | HashEqual u -> sprintf "W.hash(%s)" (u.ToString())
        | Time t -> sprintf "W.time(%d)" t
        | CastE e -> e.print()

and F with
    member this.print() =
        match this with
        | CheckSig pk -> sprintf "F.pk(%s)" (pk.ToString())
        | CheckMultiSig(m, pks) -> 
            sprintf "F.multi(%d,%s)" m 
                (pks 
                 |> Array.fold (fun acc k -> sprintf "%s,%s" acc (k.ToString())) 
                        "")
        | Time t -> sprintf "F.time(%d)" t
        | HashEqual h -> sprintf "F.hash(%s)" (h.ToString())
        | Threshold(num, e, ws) -> 
            sprintf "F.thres(%d,%s,%s)" num (e.print()) 
                (ws 
                 |> Array.fold (fun acc w -> sprintf "%s,%s" acc (w.print())) "")
        | And(l, r) -> sprintf "F.and(%s,%s)" (l.print()) (r.print())
        | CascadeOr(l, r) -> sprintf "F.or_v(%s,%s)" (l.print()) (r.print())
        | SwitchOr(l, r) -> sprintf "F.or_s(%s,%s)" (l.print()) (r.print())
        | SwitchOrV(l, r) -> sprintf "F.or_a(%s,%s)" (l.print()) (r.print())
        | DelayedOr(l, r) -> sprintf "F.or_d(%s,%s)" (l.print()) (r.print())

and V with
    member this.print() =
        match this with
        | CheckSig pk -> sprintf "V.pk(%s)" (pk.ToString())
        | CheckMultiSig(m, pks) -> 
            sprintf "V.multi(%d,%s)" m 
                (pks 
                 |> Array.fold (fun acc k -> sprintf "%s,%s" acc (k.ToString())) 
                        "")
        | Time t -> sprintf "V.time(%d)" t
        | HashEqual h -> sprintf "V.hash(%s)" (h.ToString())
        | Threshold(num, e, ws) -> 
            sprintf "V.thres(%d,%s,%s)" num (e.print()) 
                (ws 
                 |> Array.fold (fun acc w -> sprintf "%s,%s" acc (w.print())) "")
        | And(l, r) -> sprintf "V.and(%s,%s)" (l.print()) (r.print())
        | CascadeOr(l, r) -> sprintf "V.or_v(%s,%s)" (l.print()) (r.print())
        | SwitchOr(l, r) -> sprintf "V.or_s(%s,%s)" (l.print()) (r.print())
        | SwitchOrT(l, r) -> sprintf "V.or_a(%s,%s)" (l.print()) (r.print())
        | DelayedOr(l, r) -> sprintf "V.or_d(%s,%s)" (l.print()) (r.print())

and T with
    member this.print() =
        match this with
        | Time t -> sprintf "T.time(%d)" t
        | HashEqual h -> sprintf "T.hash(%s)" (h.ToString())
        | And(l, r) -> sprintf "T.and_p(%s,%s)" (l.print()) (r.print())
        | ParallelOr(l, r) -> sprintf "T.or_vp(%s,%s)" (l.print()) (r.print())
        | CascadeOr(l, r) -> sprintf "T.or_c(%s,%s)" (l.print()) (r.print())
        | CascadeOrV(l, r) -> sprintf "T.or_v(%s,%s)" (l.print()) (r.print())
        | SwitchOr(l, r) -> sprintf "T.or_s(%s,%s)" (l.print()) (r.print())
        | SwitchOrV(l, r) -> sprintf "T.or_a(%s,%s)" (l.print()) (r.print())
        | DelayedOr(l, r) -> sprintf "T.or_d(%s,%s)" (l.print()) (r.print())
        | CastE e -> sprintf "T.%s" (e.print())

type AST with
    
    member this.Print() =
        match this with
        | ETree e -> e.print()
        | QTree q -> q.print()
        | WTree w -> w.print()
        | FTree f -> f.print()
        | VTree v -> v.print()
        | TTree t -> t.print()
    
    member this.castT() : Result<T, string> =
        match this with
        | TTree t -> Ok t
        | _ -> Error(sprintf "failed to cast %s" (this.Print()))
    
    member this.castE() : Result<E, string> =
        match this with
        | ETree e -> Ok e
        | _ -> Error(sprintf "failed to cast %s" (this.Print()))
    
    member this.castQ() : Result<Q, string> =
        match this with
        | QTree q -> Ok q
        | _ -> Error(sprintf "failed to cast %s" (this.Print()))
    
    member this.castW() : Result<W, string> =
        match this with
        | WTree w -> Ok w
        | _ -> Error(sprintf "failed to cast %s" (this.Print()))
    
    member this.castF() : Result<F, string> =
        match this with
        | FTree f -> Ok f
        | _ -> Error(sprintf "failed to cast %s" (this.Print()))
    
    member this.castV() : Result<V, string> =
        match this with
        | VTree v -> Ok v
        | _ -> Error(sprintf "failed to cast %s" (this.Print()))
    
    member this.castTUnsafe() : T =
        match this.castT() with
        | Ok t -> t
        | Error s -> failwith s
    
    member this.castEUnsafe() : E =
        match this.castE() with
        | Ok e -> e
        | Error s -> failwith s
    
    member this.castQUnsafe() : Q =
        match this.castQ() with
        | Ok q -> q
        | Error s -> failwith s
    
    member this.castWUnsafe() : W =
        match this.castW() with
        | Ok w -> w
        | Error s -> failwith s
    
    member this.castFUnsafe() : F =
        match this.castF() with
        | Ok f -> f
        | Error s -> failwith s
    
    member this.castVUnsafe() : V =
        match this.castV() with
        | Ok v -> v
        | Error s -> failwith s
