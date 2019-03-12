module MiniScriptCompilerTests
open Expecto
open Expecto.Logging

open Expecto.Logging.Message
open FNBitcoin.Tests.Generators

open FNBitcoin.MiniScriptCompiler
open FNBitcoin.MiniScriptParser

let logger = Log.create "MiniscriptCompiler"

let config = {
            FsCheckConfig.defaultConfig with
                arbitrary = [typeof<Generators>]
                maxTest = 30
                endSize = 128
                receivedArgs = fun _ name no args ->
                  logger.debugWithBP (
                    eventX "For {test} {no}, generated {args}"
                    >> setField "test" name
                    >> setField "no" no
                    >> setField "args" args)
        }

[<Tests>]
let tests =
    testList "miniscript compiler" [
        testPropertyWithConfig config "should compile arbitrary input" <| fun (p: Policy) ->
            let node = CompiledNode.fromPolicy(p)
            let t = node.compile()
            printfn "%s" (t.ast.Print())
    ]