namespace FNBitcoin.MiniScript
open MiniscriptAST

type MiniScript = MiniScript of AST

module MiniScript =
    let create (t: AST): Result<MiniScript, string> =
        match t with
        | TTree t -> Ok (MiniScript (TTree t))
        | _ -> Error "AST was not top-level (T) representation" 

    let fromScript (s: NBitcoin.Script) =
           failwith "not implemented!"