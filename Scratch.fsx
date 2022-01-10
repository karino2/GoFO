#r "nuget: FParsec"
open FParsec

#load "Common.fs"
open Common

let target = """1.25"""


#load "Parser.fs"
open Parser

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pidentifier target
test pstringLiteral "\"abc\""

test pexpr "$1.isFile"
test pexpr "$1.Length > 200"
test pexpr "\"/home/test\""
test pexpr "$1.Length >= 200"


test pfuncall "filter($1.IsFile)"


#load "Functions.fs"
open System.IO

Functions.ls "./" |> Seq.toList

#load "Interpreter.fs"
Interpreter.eval """ls("./")"""

Interpreter.eval """ls("./") | filter($1.IsFile)"""
Interpreter.eval """ls("./")
| filter($1.IsFile)
"""

Interpreter.eval """ls("./")
| filter($1.IsFile)
| filter($1.Length > 400)"""



//
// Try and error
//

let di = DirectoryInfo("./")

di.EnumerateFiles() |> Seq.toList

let temp = di.EnumerateFileSystemInfos() |> Seq.toList


temp.[0]
temp.[1]

temp.[1].Attributes = FileAttributes.Normal

let temp2 = [1]

temp2.[0]
temp2.[1]

let temp3 = di.EnumerateFiles() |> Seq.take 1 |> Seq.toList
temp3.[0].Name

List.append [1; 2; 3] [4]