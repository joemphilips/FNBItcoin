namespace FNBitcoin.MiniScript

open FNBitcoin.MiniScriptAST
open FNBitcoin.MiniScriptDecompiler
open NBitcoin

/// wrapper for top-level AST
type MiniScript = MiniScript of AST

module MiniScript =
    let fromAST (t : AST) : Result<MiniScript, string> =
        match t with
        | TTree t -> Ok(MiniScript(TTree t))
        | _ -> Error "AST was not top-level (T) representation"
    
    let toAST (m : MiniScript) =
        match m with
        | MiniScript a -> a

    let fromScriptUnsafe (s : NBitcoin.Script) =
        let res = parseScriptUnsafe s
        match fromAST res with
        | Ok r -> r
        | Error e -> failwith e

    let toScript (m : MiniScript) : Script =
        let ast = toAST m
        ast.ToScript()

type MiniScript with
    member this.ToScript() = MiniScript.toScript this
    member this.ToAST() = MiniScript.toAST this
