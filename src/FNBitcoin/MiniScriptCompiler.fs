module FNBitcoin.MiniScriptCompiler
open NBitcoin
open FNBitcoin.MiniScriptParser
open MiniscriptAST


type CompiledNode =
    | Pk of NBitcoin.PubKey
    | Multi of uint32 * PubKey[]
    | Hash of uint256
    | Time of uint32
    | Threshold of uint32 * CompiledNode[]
    | And of left: CompiledNode * right: CompiledNode
    | Or of left: CompiledNode * right: CompiledNode * leftProb: float * rightProb: float

type Cost = {
        ast: AST
        pkCost: uint32
        satCost: float
        dissatCost: float
    }

/// Intermediary value before computing parent Cost
type CostTriple = {
        parent: AST
        left: Cost
        right: Cost
        /// In case of F ast, we can tell the compiler that
        /// it can be combined as an E expression in two ways.
        /// This is equivalent to `->` in this macro
        /// ref: https://github.com/apoelstra/rust-miniscript/blob/ac36d4bacd6440458a57b4bd2013ea1c27058709/src/policy/compiler.rs#L333
        condCombine: bool 
    }

module Cost =
    /// Casts F -> E
    let likely (fcost: Cost): Cost =
        {
            ast=ETree(E.Likely(fcost.ast.castFUnsafe()))
            pkCost=fcost.pkCost + 4u
            satCost=fcost.satCost * 1.0
            dissatCost=2.0
        }
    let unlikely (fcost: Cost): Cost =
        {
            ast=ETree(E.Unlikely(fcost.ast.castFUnsafe()))
            pkCost=fcost.pkCost + 4u
            satCost=fcost.satCost * 2.0
            dissatCost=1.0
        }
    let fromPairToTCost (left:Cost)
                 (right: Cost)
                 (newAst: T)
                 (lweight: float)
                 (rweight: float) =
        match newAst with
        | _ -> failwith "notimplemented"

    let fromPairToQCost (left:Cost)
                 (right: Cost)
                 (newAst: Q)
                 (lweight: float)
                 (rweight: float) =
        match newAst with
        | _ -> failwith "notimplemented"

    let fromPairToFCost (left:Cost)
                 (right: Cost)
                 (newAst: F)
                 (lweight: float)
                 (rweight: float) =
        match newAst with
        | F.CheckSig _
        | F.CheckMultiSig _
        | F.Time _
        | F.Threshold _ -> failwith "unreachable"
        | F.And _ -> 
            {
                ast=FTree newAst
                pkCost=left.pkCost + right.pkCost
                satCost=left.satCost + right.satCost
                dissatCost=0.0;
            }

    let fromPairToECost (left:Cost)
                 (right: Cost)
                 (newAst: E)
                 (lweight: float)
                 (rweight: float) =
        let pkCost = left.pkCost + right.pkCost
        match newAst with
        | E.CheckSig _
        | E.CheckMultiSig _
        | E.Time _
        | E.Threshold _
        | E.Likely _
        | E.Unlikely _
        | E.ParallelAnd _ ->
            {
                ast=ETree newAst
                pkCost=pkCost + 1u
                satCost =  left.satCost + right.satCost
                dissatCost = left.dissatCost + right.dissatCost
            }
        | E.CascadeAnd _ ->
            {
                ast=ETree newAst
                pkCost=pkCost + 4u
                satCost=left.satCost + right.satCost
                dissatCost=left.dissatCost
            }
        | E.ParallelOr _ ->
            {
                ast=ETree newAst
                pkCost=pkCost + 1u
                satCost=left.satCost * lweight + (right.satCost + left.dissatCost) * rweight
                dissatCost = left.dissatCost + right.dissatCost
            }
        | E.CascadeOr _ ->
            {
                ast=ETree newAst
                pkCost=left.pkCost + right.pkCost + 3u
                satCost=left.satCost * lweight + (right.satCost + left.dissatCost) * rweight
                dissatCost = left.dissatCost + right.dissatCost
            }
        | E.SwitchOrLeft _ ->
            {
                ast=ETree newAst
                pkCost=pkCost + 3u
                satCost=(left.satCost + 2.0) * lweight + (right.satCost + 1.0) * rweight
                dissatCost=left.dissatCost + 2.0
            }
        | E.SwitchOrRight _ ->
            {
                ast=ETree newAst
                pkCost=pkCost + 3u
                satCost=(left.satCost + 1.0) * lweight + (right.satCost + 2.0) * rweight
                dissatCost=left.dissatCost + 1.0
            }

    // TODO: Consider carefully about where swap case should be treated.
    let fromTriple (triple: CostTriple)
                   (lweight: float)
                   (rweight: float): Cost[] =
        match triple.parent with
        | TTree t ->
            let cost = fromPairToTCost triple.left triple.right t lweight rweight
            let costSwap = fromPairToTCost triple.right triple.left t rweight lweight
            [|cost; costSwap|]
        | ETree e ->
            let cost = fromPairToECost triple.left triple.right e lweight rweight
            let costSwap = fromPairToECost triple.right triple.left e rweight lweight
            [|cost; costSwap|]
        | FTree f ->
            match triple.condCombine with
            | (false) ->
                let cost = fromPairToFCost triple.left triple.right f lweight rweight
                let costSwap = fromPairToFCost triple.right triple.left f rweight lweight
                [|cost; costSwap|]
            | (true) ->
                let costBeforeCast = fromPairToFCost triple.left triple.right f lweight rweight
                let costBeforeCastSwap = fromPairToFCost triple.right triple.left f rweight lweight
                [|
                    likely(costBeforeCast)
                    unlikely(costBeforeCast)
                    likely(costBeforeCastSwap)
                    unlikely(costBeforeCastSwap)
                |]
        | QTree q ->
            let cost = fromPairToQCost triple.left triple.right q lweight rweight
            let costSwap = fromPairToQCost triple.right triple.left q rweight lweight
            [|cost; costSwap|]

    let min_cost(a: Cost, b: Cost, p_sat: float, p_dissat: float) =
        let weight_one = (float a.pkCost) + p_sat * a.satCost + p_dissat * a.dissatCost
        let weight_two = (float b.pkCost) + p_sat * b.satCost + p_dissat * a.dissatCost
        if weight_one < weight_two then a else
        if weight_one > weight_two then b else
        if a.satCost < b.satCost then a else
            b

    let fold_costs (p_sat: float) (p_dissat: float) (cs: Cost[]) =
        cs |> Array.toList |>List.reduce(fun a b -> min_cost(a, b, p_sat, p_dissat))

    // equivalent to rules! macro in rust-miniscript
    let getMinimumCost (triples: CostTriple[])
                         (p_sat)
                         (p_dissat)
                         (lweight: float)
                         (rweight: float): Cost =
        triples
            |> Array.map(fun p -> fromTriple p 0.0 0.0)
            |> Array.concat
            |> fold_costs p_sat p_dissat

module CompiledNode =
    let private scriptNumCost n =
        if n <= 16u then
            1u
        else if n < 0x100u then
            2u
        else if n < 0x10000u then
            3u
        else if n < 0x1000000u then
            4u
        else
            5u

    let private minCost(one: Cost, two: Cost, p_sat: float, p_dissat) =
        let weight_one = (float one.pkCost) + p_sat * one.satCost + p_dissat * one.dissatCost
        let weight_two = (float two.pkCost) + p_sat * two.satCost + p_dissat * two.dissatCost
        if weight_one < weight_two then
            one
        else if weight_two < weight_one then
            one
        else if one.satCost < two.satCost then
            one
        else
            two

    let rec fromPolicy (p: Policy): CompiledNode = 
        match p with
        | Key k -> Pk k
        | Policy.Multi(m, pks) -> Multi (m, pks)
        | Policy.Hash h -> Hash h
        | Policy.Time t -> Time t
        | Policy.Threshold (n, subexprs) ->
            let ps = subexprs |> Array.map fromPolicy
            Threshold (n, ps)
        | Policy.And(e1, e2) -> And(fromPolicy e1, fromPolicy e2)
        | Policy.Or(e1, e2) -> Or(fromPolicy e1, fromPolicy e2, 0.5, 0.5)
        | Policy.AsymmetricOr(e1, e2) -> Or(fromPolicy e1, fromPolicy e2, 127.0/128.0 , 1.0/128.0)

    let rec best_t (node: CompiledNode) (p_sat: float) (p_dissat: float): Cost =
        match node with
        | Pk _ | Multi _ | Threshold _ ->
            let e = best_e node p_sat p_dissat
            { ast = e.ast; pkCost = e.pkCost; satCost = e.satCost; dissatCost = 0.0 }
        | Time t ->
            let num_cost = NBitcoin.Op.GetPushOp(int64 t).ToBytes().Length
            { ast=TTree (T.Time t) ; pkCost=1u + uint32 num_cost; satCost=0.0; dissatCost=0.0 }
        | Hash h ->
            { ast=TTree (T.HashEqual h); pkCost=39u; satCost=33.0; dissatCost=0.0 }
        | And (l, r) ->
            let vl = best_v l p_sat 0.0
            let vr = best_v r p_sat 0.0
            let tl = best_t l p_sat 0.0
            let tr = best_t r p_sat 0.0
            let possibleCases = [|
                    {
                        parent= TTree(T.And(vl.ast.castVUnsafe(), tr.ast.castTUnsafe()))
                        left=vl
                        right=tr
                        condCombine=false
                    }
                    {
                        parent= TTree(T.And(vr.ast.castVUnsafe(), tl.ast.castTUnsafe()))
                        left=vl
                        right=tr
                        condCombine=false
                    }
                |]
            Cost.getMinimumCost possibleCases p_sat p_dissat 0.0 0.0
        | Or (l, r, lweight, rweight) ->
            let le = best_e l (p_sat * lweight) (p_sat * rweight)
            let re = best_e r (p_sat * rweight) (p_sat * lweight)
            let lw = best_w l (p_sat * lweight) (p_sat * rweight)
            let rw = best_w r (p_sat * rweight) (p_sat * lweight)

            let lt = best_t l (p_sat * lweight) 0.0
            let rt = best_t r (p_sat * lweight) 0.0
            let lv = best_v l (p_sat * lweight) 0.0
            let rv = best_v r (p_sat * lweight) 0.0
            let lq = best_q l (p_sat * lweight) 0.0
            let rq = best_q r (p_sat * lweight) 0.0

            let possibleCases = [|
                {
                    parent=TTree(T.ParallelOr(le.ast.castEUnsafe(), rw.ast.castWUnsafe()))
                    left=le
                    right=rw
                    condCombine=false
                }
                {
                    parent=TTree(T.ParallelOr(re.ast.castEUnsafe(), lw.ast.castWUnsafe()))
                    left=re
                    right=lw
                    condCombine=false
                }
                {
                    parent=TTree(T.CascadeOr(le.ast.castEUnsafe(), rt.ast.castTUnsafe()))
                    left=le
                    right=rt
                    condCombine=false
                }
                {
                    parent=TTree(T.CascadeOr(re.ast.castEUnsafe(), lt.ast.castTUnsafe()))
                    left=re
                    right=lt
                    condCombine=false
                }
                {
                    parent=TTree(T.CascadeOrV(le.ast.castEUnsafe(), rv.ast.castVUnsafe()))
                    left=le
                    right=rv
                    condCombine=false
                }
                {
                    parent=TTree(T.CascadeOrV(re.ast.castEUnsafe(), lv.ast.castVUnsafe()))
                    left=re
                    right=lv
                    condCombine=false
                }
                {
                    parent=TTree(T.SwitchOr(lt.ast.castTUnsafe(), rt.ast.castTUnsafe()))
                    left=lt
                    right=rt
                    condCombine=false
                }
                {
                    parent=TTree(T.SwitchOr(rt.ast.castTUnsafe(), lt.ast.castTUnsafe()))
                    left=rt
                    right=lt
                    condCombine=false
                }
                {
                    parent=TTree(T.SwitchOrV(lv.ast.castVUnsafe(), rv.ast.castVUnsafe()))
                    left=lv
                    right=rv
                    condCombine=false
                }
                {
                    parent=TTree(T.SwitchOrV(rv.ast.castVUnsafe(), lv.ast.castVUnsafe()))
                    left=rv
                    right=lv
                    condCombine=false
                }
                {
                    parent=TTree(T.DelayedOr(lq.ast.castQUnsafe(), rq.ast.castQUnsafe()))
                    left=lq
                    right=rq
                    condCombine=false
                }
            |]
            Cost.getMinimumCost possibleCases p_sat 0.0 lweight rweight


    and best_e (node: CompiledNode) (p_sat: float) (p_dissat: float): Cost =
        match node with
        | Pk k -> 
            { ast = ETree(E.CheckSig k); pkCost = 35u; satCost = 72.0; dissatCost = 1.0 }
        | Multi (m, pks) ->
            let num_cost = match (m > 16u, pks.Length > 16) with
                           | (true, true) -> 4
                           | (true, false) -> 3
                           | (false, true) -> 3
                           | (false, false) -> 2
            let options = [{
                    ast=ETree(E.CheckMultiSig(m, pks))
                    pkCost=uint32 (num_cost + 34 * pks.Length + 1)
                    satCost=2.0
                    dissatCost=1.0
                }]
            if not (p_dissat > 0.0) then
                options.[0]
            else
                let bestf = best_f node p_sat 0.0
                let options2 = [Cost.likely(bestf); Cost.unlikely(bestf)]
                List.concat[ options; options2 ]
                    |> List.toArray |> Cost.fold_costs p_sat p_dissat
        | Time n ->
            let num_cost = scriptNumCost n
            { ast=ETree(E.Time n); pkCost=5u + num_cost; satCost=2.0; dissatCost=1.0}
        | Hash h ->
            let fcost = best_f node p_sat p_dissat
            minCost(Cost.likely fcost, Cost.unlikely fcost, p_sat, p_dissat)
        | And (l, r) ->
            let le = best_e l p_sat p_dissat
            let re = best_e r p_sat p_dissat
            let lw = best_w l p_sat p_dissat
            let rw = best_w r p_sat p_dissat

            let lf = best_f l p_sat 0.0
            let rf = best_f r p_sat 0.0
            let lv = best_v l p_sat 0.0
            let rv = best_v r p_sat 0.0

            let possibleCases = [|
                {
                    parent=ETree(E.ParallelAnd(le.ast.castEUnsafe(), rw.ast.castWUnsafe()))
                    left=le
                    right=rw
                    condCombine=false
                }
                {
                    parent=ETree(E.ParallelAnd(re.ast.castEUnsafe(), lw.ast.castWUnsafe()))
                    left=re
                    right=lw
                    condCombine=false
                }
                {
                    parent=ETree(E.CascadeAnd(le.ast.castEUnsafe(), rf.ast.castFUnsafe()))
                    left=le
                    right=rf
                    condCombine=false
                }
                {
                    parent=ETree(E.CascadeAnd(re.ast.castEUnsafe(), lf.ast.castFUnsafe()))
                    left=re
                    right=lf
                    condCombine=false
                }
                {
                    parent=FTree(F.And(lv.ast.castVUnsafe(), rf.ast.castFUnsafe()))
                    left=lv
                    right=rf
                    condCombine=true
                }
                {
                    parent=FTree(F.And(rv.ast.castVUnsafe(), lf.ast.castFUnsafe()))
                    left=rv
                    right=lf
                    condCombine=true
                }
            |]
            Cost.getMinimumCost possibleCases p_sat p_dissat 0.5 0.5
        | Or (l, r, lweight, rweight) -> 
            let le_par = best_e l (p_sat * lweight) (p_dissat + p_sat * rweight)
            let re_par = best_e r (p_sat * lweight) (p_dissat + p_sat * rweight)
            let lw_par = best_w l (p_sat * lweight) (p_dissat + p_sat * rweight)
            let rw_par = best_w r (p_sat * lweight) (p_dissat + p_sat * rweight)
            let le_cas = best_e l (p_sat * lweight) (p_dissat)
            let re_cas = best_e r (p_sat * lweight) (p_dissat)

            let le_cond_par = best_e l (p_sat * lweight) (p_sat * rweight)
            let re_cond_par = best_e r (p_sat * lweight) (p_sat * lweight)

            let lv = best_v l (p_sat * lweight) 0.0
            let rv = best_v r (p_sat * rweight) 0.0
            let lf = best_f l (p_sat * lweight) 0.0
            let rf = best_f r (p_sat * rweight) 0.0
            let lq = best_q l (p_sat * lweight) 0.0
            let rq = best_q r (p_sat * rweight) 0.0
            let possibleCases = [|
                {
                    parent=ETree(E.ParallelOr(le_par.ast.castEUnsafe(), rw_par.ast.castWUnsafe()))
                    left=le_par
                    right=rw_par
                    condCombine=false
                }
                {
                    parent=ETree(E.ParallelOr(re_par.ast.castEUnsafe(), lw_par.ast.castWUnsafe()))
                    left=re_par
                    right=lw_par
                    condCombine=false
                }
                {
                    parent=ETree(E.CascadeOr(le_par.ast.castEUnsafe(), re_cas.ast.castEUnsafe()))
                    left=le_par
                    right=re_cas
                    condCombine=false
                }
                {
                    parent=ETree(E.CascadeOr(re_par.ast.castEUnsafe(), le_cas.ast.castEUnsafe()))
                    left=re_par
                    right=le_cas
                    condCombine=false
                }
                {
                    parent=ETree(E.SwitchOrLeft(le_cas.ast.castEUnsafe(), rf.ast.castFUnsafe()))
                    left=le_cas
                    right=rf
                    condCombine=false
                }
                {
                    parent=ETree(E.SwitchOrLeft(re_cas.ast.castEUnsafe(), lf.ast.castFUnsafe()))
                    left=re_cas
                    right=lf
                    condCombine=false
                }
                {
                    parent=ETree(E.SwitchOrRight(le_cas.ast.castEUnsafe(), rf.ast.castFUnsafe()))
                    left=le_cas
                    right=rf
                    condCombine=false
                }
                {
                    parent=ETree(E.SwitchOrRight(re_cas.ast.castEUnsafe(), lf.ast.castFUnsafe()))
                    left=re_cas
                    right=lf
                    condCombine=false
                }

                {
                    parent=FTree(F.CascadeOr(le_cas.ast.castEUnsafe(), rv.ast.castVUnsafe()))
                    left=le_cas
                    right=rv
                    condCombine=true
                }
                {
                    parent=FTree(F.CascadeOr(re_cas.ast.castEUnsafe(), lv.ast.castVUnsafe()))
                    left=re_cas
                    right=lv
                    condCombine=true
                }
                {
                    parent=FTree(F.SwitchOr(lf.ast.castFUnsafe(), rf.ast.castFUnsafe()))
                    left=lf
                    right=rf
                    condCombine=true
                }
                {
                    parent=FTree(F.SwitchOr(rf.ast.castFUnsafe(), lf.ast.castFUnsafe()))
                    left=rf
                    right=lf
                    condCombine=true
                }
                {
                    parent=FTree(F.SwitchOrV(lv.ast.castVUnsafe(), rv.ast.castVUnsafe()))
                    left=lv
                    right=rv
                    condCombine=true
                }
                {
                    parent=FTree(F.SwitchOrV(rv.ast.castVUnsafe(), lv.ast.castVUnsafe()))
                    left=rv
                    right=lv
                    condCombine=true
                }
                {
                    parent=FTree(F.DelayedOr(lq.ast.castQUnsafe(), rq.ast.castQUnsafe()))
                    left=lq
                    right=rq
                    condCombine=true
                }
                |]
            Cost.getMinimumCost possibleCases p_sat p_dissat lweight rweight

        | Threshold (n, subs) -> 
            let num_cost = scriptNumCost n
            let avgCost = float n / float subs.Length
            let e = best_e subs.[0] (p_sat * avgCost) (p_dissat + p_sat * (1.0 - avgCost))

            let ws = subs
                     |> Array.map(fun s -> best_w s (p_sat * avgCost) (p_dissat + p_sat * (1.0 - avgCost)) )

            let pk_cost = ws |> Array.fold(fun acc w -> acc + w.pkCost) (1u + num_cost + e.pkCost)
            let sat_cost = ws |> Array.fold(fun acc w -> acc + w.satCost) e.satCost
            let dissat_cost = ws |> Array.fold(fun acc w -> acc + w.dissatCost) e.dissatCost
            let wsast = ws |> Array.map(fun w -> w.ast.castWUnsafe())
            let cond = {
                    ast=ETree(E.Threshold(n, e.ast.castEUnsafe(), wsast))
                    pkCost=pk_cost
                    satCost=sat_cost
                    dissatCost=dissat_cost
                }

            let f = best_f node p_sat 0.0
            let cond1 = Cost.likely(f)
            let cond2 = Cost.unlikely(f)
            let nonCond = Cost.min_cost(cond1, cond2, p_sat, p_dissat)
            Cost.min_cost(cond, nonCond, p_sat, p_dissat)
    and best_q (node: CompiledNode) (p_sat: float) (p_dissat: float): Cost =
        failwith "not impl"
    and best_w (node: CompiledNode) (p_sat: float) (p_dissat: float): Cost =
        failwith "not impl"
    and best_f (node: CompiledNode) (p_sat: float) (p_dissat: float): Cost =
        failwith "not impl"
    and best_v (node: CompiledNode) (p_sat: float) (p_dissat: float): Cost =
        failwith "not impl"

type CompiledNode with
    static member fromPolicy(p: Policy) = CompiledNode.fromPolicy p