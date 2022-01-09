open System
open FParsec

let target = """1.25"""


let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg


[<EntryPoint>]
let main argv =
    test pfloat target
    0 // return an integer exit code