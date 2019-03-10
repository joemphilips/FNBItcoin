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
    | Or of left: CompiledNode * right: CompiledNode * prob1: float * prob2: float

type Cost = {
        ast: AST
        pkCost: uint32
        satCost: float
        dissatCost: float
    }

type CostTriple = {
        parent: AST
        left: Cost
        right: Cost
    }

module Cost =
    let fromPairToT (left:Cost)
                 (right: Cost)
                 (newAst: T)
                 (lweight: float)
                 (rweight: float): Cost =
        match newAst with
        | _ -> failwith "notimplemented"

    let fromPairToE (left:Cost)
                 (right: Cost)
                 (newAst: E)
                 (lweight: float)
                 (rweight: float): Cost =
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
    let fromTriple (triple: CostTriple)
                   (lweight: float)
                   (rweight: float): Cost =
        match triple.parent with
        | TTree t -> fromPairToT triple.left triple.right t lweight rweight
        | ETree e -> fromPairToE triple.left triple.right e lweight rweight

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
            |> fold_costs p_sat p_dissat

module CompiledNode =
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
                    }
                    {
                        parent= TTree(T.And(vr.ast.castVUnsafe(), tl.ast.castTUnsafe()))
                        left=vl
                        right=tr
                    }
                |]
            Cost.getMinimumCost possibleCases p_sat p_dissat 0.0 0.0


    and best_e (node: CompiledNode) (p_sat: float) (p_dissat: float): Cost =
        failwith "not impl"
    and best_v (node: CompiledNode) (p_sat: float) (p_dissat: float): Cost =
        failwith "not impl"

type CompiledNode with
    static member fromPolicy(p: Policy) = CompiledNode.fromPolicy p