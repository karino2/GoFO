module Functions

open Common
open System.IO

let ls path =
    DirectoryInfo(path).EnumerateFileSystemInfos()
    |> Seq.map (fun x-> Row [Value.File x])

let filter expr valseq =
    // printfn "filter called"
    valseq |> Seq.filter (fun row -> Eval.toBoolean row expr)

let mutate expr valseq =
    // printfn "filter called"
    valseq |> Seq.map (fun row -> Eval.toValue row expr |> addColumn row)
