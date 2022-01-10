module Functions

open Common
open System.IO

let ls path =
    DirectoryInfo(path).EnumerateFileSystemInfos()
    |> Seq.map (fun x-> Value.File x)

let filter expr valseq =
    // printfn "filter called"
    valseq |> Seq.filter (fun v -> Eval.toBoolean expr v)
