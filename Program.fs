
let target =  """ls("./")
| filter($1.isFile)
| filter($1.Length > 400)"""


[<EntryPoint>]
let main argv =
    Interpreter.eval target |> printfn "%A"

    0 // return an integer exit code