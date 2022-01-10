module Interpreter

open Common
open Parser
open FParsec

let callToplevelFun funid args =
    match funid, args with
    | "ls", x::[] -> Eval.toStringTopLevel x |> Functions.ls
    | _-> failwith("unknwon func")


let callPipedFun (pfun:Funcall) (valseq:seq<Row>) =
    match pfun with
    | {Identifier="filter"; Args=arg::[]} -> Functions.filter arg valseq
    | {Identifier="mutate"; Args=arg::[]} -> Functions.mutate arg valseq
    | {Identifier=x; Args=_} -> failwith($"Unsupported pieped function: {x}")


let pinterpreter = pPipeline (fun (fcalls:list<Funcall>) ->
            // printfn "deb1 %A" fcalls
            match fcalls with
            |head::rest ->
                let {Funcall.Identifier=fid; Args= args} = head
                let first = callToplevelFun fid args
                // printfn "deb2 %A" rest
                (first, rest) ||> List.fold (fun acc pfun -> callPipedFun pfun acc)
            |[] -> Seq.empty
        )

let eval script =
    run pinterpreter script