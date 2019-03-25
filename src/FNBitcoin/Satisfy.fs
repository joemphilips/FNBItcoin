namespace FNBitcoin.Satisfy

module Satisfy =
    open FNBitcoin.MiniScriptAST
    open FNBitcoin.Utils
    open NBitcoin
    open System

    type SignatureProvider = PubKey -> TransactionSignature option
    type PreImageHash = PreImagehash of uint256
    type PreImage = PreImage of uint256
    type PreImageProvider = PreImageHash -> PreImage

    type ProviderSet = (SignatureProvider * PreImageProvider * TimeSpan)

    type CSVOffset = BlockHeight of uint32 | UnixTime of DateTimeOffset
    type FailureCase =
        | MissingSig of PubKey list
        | NotMatured of CSVOffset
        | LockTimeTypeMismatch 
        | Nested of FailureCase list

    type SatisfiedItem =
        | PreImage of byte[]
        | Signature of TransactionSignature
        | OtherPush of byte[]

    type SatisfactionResult = Result<SatisfiedItem list, FailureCase>

    let satisfyCost (res: SatisfiedItem list): int = failwith ""

    let (>>=) xR f = Result.bind f xR

    // ------- helpers --------
    let satisfyCheckSig (keyFn: SignatureProvider) k =
        match keyFn k with
        | None -> Error (MissingSig [k])
        | Some(txSig) -> Ok([Signature(txSig)])

    let satisfyCheckMultisig(m, pks, keyFn: SignatureProvider) =
        let maybeSigList = pks
                           |> Array.map(keyFn)
                           |> Array.toList

        let sigList = maybeSigList |> List.choose(id) |> List.map(Signature)

        if sigList.Length >= (int32 m) then
            Ok(sigList)
        else
            let sigNotFoundPks = maybeSigList
                                 |> List.zip (pks |> Array.toList)
                                 |> List.choose(fun (pk, maybeSig) ->
                                                    if maybeSig.IsNone then Some(pk) else None)
            Error(MissingSig(sigNotFoundPks))

    let satisfyCSV(t: LockTime, age: LockTime) =
        let offset = t.Value - age.Value
        if
            (age.IsHeightLock && t.IsHeightLock)
        then
            if (offset > 0u) then
                Error(NotMatured(BlockHeight offset))
            else
                Ok([])
        else if
            (age.IsTimeLock && t.IsTimeLock)
        then
            if (offset > 0u) then
                Error(NotMatured(UnixTime(DateTimeOffset.FromUnixTimeSeconds(int64 (offset)))))
            else
                Ok([])
        else
            Error(LockTimeTypeMismatch)

    let rec satisfyThreshold(k, e, ws: W[], keyFn, hashFn, age): SatisfactionResult =
        let flatten l = List.collect id l
        let wsList = ws |> Array.toList

        let wResult = wsList
                      |> List.rev
                      |> List.map(satisfyW(keyFn, hashFn, age))
        let wOkList = wResult
                      |> List.filter(fun wr -> match wr with | Ok w -> true;| _ -> false)
                      |> List.map(fun wr -> match wr with | Ok w -> w; | _ -> failwith "unreachable")

        let wErrorList = wResult
                         |> List.filter(fun wr -> match wr with | Error w -> true;| _ -> false)
                         |> List.map(fun wr -> match wr with | Error e -> e; | _ -> failwith "unreachable")

        let eResult = satisfyE (keyFn, hashFn, age) e |> List.singleton
        let eOkList = eResult
                      |> List.filter(fun wr -> match wr with | Ok w -> true;| _ -> false)
                      |> List.map(fun wr -> match wr with | Ok w -> w; | _ -> failwith "unreachable")

        let eErrorList = eResult
                         |> List.filter(fun wr -> match wr with | Error w -> true;| _ -> false)
                         |> List.map(fun wr -> match wr with | Error e -> e; | _ -> failwith "unreachable")


        let satisfiedTotal = wOkList.Length + eOkList.Length

        if satisfiedTotal >= (int k) then
            let dissatisfiedW = List.zip wsList wResult
                                |> List.choose(fun (w, wr) -> match wr with | Error _ -> Some(w); | _ -> None)
                                |> List.map(dissatisfyW)
            let dissatisfiedE = match eResult.[0] with | Error _ -> [dissatisfyE e] | Ok _ -> []
            Ok(flatten (wOkList @ eOkList @ dissatisfiedW @ dissatisfiedE))
        else
            Error(Nested(wErrorList @ eErrorList))

    and satisfyAST (keyFn, hashFn, age) (ast: AST) =
        match ast.GetASTType() with
        | EExpr -> satisfyE (keyFn, hashFn, age) (ast.castEUnsafe())
        | FExpr -> satisfyF (keyFn, hashFn, age) (ast.castFUnsafe())
        | WExpr -> satisfyW (keyFn, hashFn, age) (ast.castWUnsafe())
        | QExpr -> satisfyQ (keyFn, hashFn, age) (ast.castQUnsafe())
        | TExpr -> satisfyT (keyFn, hashFn, age) (ast.castTUnsafe())
        | VExpr -> satisfyV (keyFn, hashFn, age) (ast.castVUnsafe())

    and dissatisfyAST (ast: AST) =
        match ast.GetASTType() with
        | EExpr -> dissatisfyE (ast.castEUnsafe())
        | WExpr -> dissatisfyW (ast.castWUnsafe())
        | _ -> failwith "unreachable"

    and satisfyParallelOr providers (l: AST, r: AST) =
        match (satisfyAST providers l), (satisfyAST providers r) with
        | Ok(lItems), Ok (rItems) -> // return the one has less cost
            let lDissat = dissatisfyAST l
            let rDissat = dissatisfyAST r
            if (satisfyCost rDissat + satisfyCost lItems <= satisfyCost rItems + satisfyCost lDissat) then
                Ok(rDissat @ lItems)
            else
                Ok(lDissat @ rItems)
        | Ok(lItems), Error _ ->
            let rDissat = dissatisfyAST r
            Ok(lItems @ rDissat)
        | Error _, Ok(rItems) ->
            let lDissat = dissatisfyAST l
            Ok(rItems @ lDissat)
        | Error e1, Error e2 -> Error(Nested([e1; e2]))

    and satisfyCascadeOr providers (l, r) =
        match (satisfyAST providers l), (satisfyAST providers r) with
        | Error e, Error _ -> Error e
        | Ok lItems, Error _ -> Ok(lItems)
        | Error _, Ok rItems ->
            let lDissat = dissatisfyAST l
            Ok(rItems @ lDissat)
        | Ok lItems, Ok rItems ->
            let lDissat = dissatisfyAST l
            if satisfyCost lItems <= satisfyCost rItems + satisfyCost lDissat then
                Ok(lItems)
            else
                Ok(rItems)

    and satisfySwitchOr providers (l, r) =
        match (satisfyAST providers l), (satisfyAST providers r) with
        | Error e, Error _ -> Error e
        | Ok lItems, Error _ -> Ok(lItems @ [OtherPush([|byte 1|])])
        | Error e, Ok rItems -> Ok(rItems @ [OtherPush([|byte 0|])])
        | Ok lItems, Ok rItems -> // return the one has less cost
            if satisfyCost(lItems) + 2 <= satisfyCost rItems + 1 then
                Ok(lItems @ [OtherPush([|byte 1|])])
            else
                Ok(rItems @ [OtherPush([|byte 0|])])

    and satisfyE(keyFn, hashFn, age) (e: E) =
        match e with
        | E.CheckSig k -> satisfyCheckSig keyFn k
        | E.CheckMultiSig(m, pks) -> satisfyCheckMultisig(m, pks, keyFn)
        | E.Time t ->  satisfyCSV(t, age)
        | E.Threshold (k, sube, subw) ->
            satisfyThreshold(k, sube, subw, keyFn, hashFn, age)
        | E.ParallelAnd(e, w) ->
            satisfyE (keyFn, hashFn, age) e
                >>= (fun eitem -> satisfyW(keyFn, hashFn, age) w >>= (fun witem -> Ok(eitem @ witem)))
        | E.CascadeAnd(e, f) ->
            satisfyE (keyFn, hashFn, age) e
                >>= (fun eitem -> satisfyF(keyFn, hashFn, age) f >>= (fun fitem -> Ok(eitem @ fitem)))
        | E.ParallelOr(e, w) -> satisfyParallelOr (keyFn, hashFn, age) (ETree(e), WTree(w))
        | E.CascadeOr(e1, e2) -> satisfyCascadeOr (keyFn, hashFn, age) (ETree(e1), ETree(e2))
        | E.SwitchOrLeft(e, f) -> satisfySwitchOr (keyFn, hashFn, age) (ETree(e), FTree(f))
        | E.SwitchOrRight(e, f) -> satisfySwitchOr (keyFn, hashFn, age) (ETree(e), FTree(f))
        | E.Likely f ->
            satisfyF (keyFn, hashFn, age) f |> Result.map(fun items -> items @ [OtherPush([||])])
        | E.Unlikely f ->
            satisfyF (keyFn, hashFn, age) f |> Result.map(fun items -> items @ [OtherPush([|byte 1|])])

    and satisfyW(keyFn, hashFn, age) w: SatisfactionResult =
        match w with
        | _ -> failwith ""

    and satisfyT(keyFn, hashFn, age) t =
        match t with
        | _ -> failwith ""

    and satisfyQ(keyFn, hashFn, age) q =
        match q with
        | _ -> failwith ""

    and satisfyF(keyFn, hashFn, age) f =
        match f with
        | _ -> failwith ""

    and satisfyV(keyFn, hashFn, age) v =
        match v with
        | _ -> failwith ""

    and dissatisfyE (e: E): SatisfiedItem list =
        match e with
        | _ -> failwith "not impl"

    and dissatisfyW (w: W): SatisfiedItem list =
        match w with
        | W.CheckSig _ -> []
        | W.HashEqual _ -> []
        | W.Time _ -> []
        | W.CastE e -> dissatisfyE e

    // ---------- types -------
    type E with
        member this.Satisfy(keyFn: SignatureProvider,
                            hashFn: PreImageProvider,
                            age: LockTime): SatisfactionResult = satisfyE(keyFn, hashFn, age) this

    type T with
        member this.Satisfy(keyFn: SignatureProvider,
                            hashFn: PreImageProvider,
                            age: LockTime): SatisfactionResult = satisfyT(keyFn, hashFn, age) this
    type W with
        member this.Satisfy(keyFn: SignatureProvider,
                            hashFn: PreImageProvider,
                            age: LockTime): SatisfactionResult = satisfyW(keyFn, hashFn, age) this
    type Q with
        member this.Satisfy(keyFn: SignatureProvider,
                            hashFn: PreImageProvider,
                            age: LockTime): SatisfactionResult = satisfyQ(keyFn, hashFn, age) this
    type F with
        member this.Satisfy(keyFn: SignatureProvider,
                            hashFn: PreImageProvider,
                            age: LockTime): SatisfactionResult = satisfyF(keyFn, hashFn, age) this
    type V with
        member this.Satisfy(keyFn: SignatureProvider,
                            hashFn: PreImageProvider,
                            age: LockTime): SatisfactionResult = satisfyV(keyFn, hashFn, age) this
