#r "nuget: FParsec"
open FParsec


let target = """1.25"""


let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pfloat "1.25"

let ws = spaces
let str_ws s = pstring s .>> ws
let float_ws = pfloat .>> ws
let numberList = str_ws "[" >>. sepBy float_ws (str_ws ",") .>> str_ws "]"

test numberList target

test numberList "[1.25, 1.2, 1]"


let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    .>> ws // skips trailing whitespace

test identifier target

let stringLiteral =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"")
            (manyChars (normalChar <|> escapedChar))

test stringLiteral "\"abc\""


open System
open System.IO

type Value = 
| File of FileSystemInfo 
| Number of int
| Float of float
| Date of DateTime
| Bool of bool
| String of string

let ls path =
    DirectoryInfo(path).EnumerateFileSystemInfos()
    |> Seq.map (fun x-> File x)

ls "./" |> Seq.toList

type Variable = SpecialVariable of int // $1, $2, etc.

type Atom =
| String of string
| Number of int
| Float of float
| Variable of Variable


type BinOpType = 
| EqOp
| GeOp
| GtOp
| LtOp 
| LeOp 

type FieldAccess = {target: Variable; field: string}

type Expr =
| Atom of Atom
| FieldAccess of FieldAccess
| BinOp of (BinOpType * Expr * Expr)

let toString  = function
| Atom atom ->
    match atom with
    | String x -> x
    | Variable _ -> failwith("Variable to String, NYI")
    | _ -> failwith("Non string value")
| BinOp _ -> failwith("NYI BinOp")
| FieldAccess _ -> failwith("NYI FieldAccess")

type Funcall = {Identifier: string; Args: Expr list}

let pterm =
    (stringLiteral |>> (fun x -> Atom (String x)))
    <|>(pfloat |>> (fun x -> Atom (Float x)))
    <|>(pint32 |>> (fun x -> Atom (Number x)))
    <|> ((pstring "$") >>. (pipe2 pint32 ((str_ws ".") >>. identifier) (fun num fname ->  FieldAccess {target= Variable.SpecialVariable num; field=fname})))
    <|> ((pstring "$") >>. (pint32 |>> (fun x ->  Atom (Variable (SpecialVariable x)))))

let pterm_ws = pterm .>> ws


let opp = new OperatorPrecedenceParser<Expr,unit,unit>()
let pexpr = opp.ExpressionParser
opp.TermParser <- pterm <|> between (str_ws "(") (str_ws ")") pexpr

opp.AddOperator(InfixOperator("==", ws, 1, Associativity.Left, fun x y -> BinOp (EqOp, x, y)))
opp.AddOperator(InfixOperator(">=", ws, 1, Associativity.Left, fun x y -> BinOp (GeOp, x, y)))
opp.AddOperator(InfixOperator(">", ws, 1, Associativity.Left, fun x y -> BinOp (GtOp, x, y)))
opp.AddOperator(InfixOperator("<", ws, 1, Associativity.Left, fun x y -> BinOp (LtOp, x, y)))
opp.AddOperator(InfixOperator("<=", ws, 1, Associativity.Left, fun x y -> BinOp (LeOp, x, y)))

test pexpr "$1.isFile"
test pexpr "$1.Length > 200"
test pexpr "\"/home/test\""
test pexpr "$1.Length >= 200"

let pargs = 
     between (pstring "(") (pstring ")")
        (sepBy pexpr (str_ws ","))

let evalToplevelFuncall funid args =
    match funid, args with
    | "ls", x::[] -> toString x |> ls
    | _-> failwith("unknwon func")


let pfuncall =
    pipe2 identifier pargs (fun funid args -> {Identifier=funid; Args = args})

test pfuncall "filter($1.isFile)"


// toValue
let evalAtomWith (atom:Atom) (curRow:Value) =
    match atom with
    | Variable (SpecialVariable 1) -> curRow // currently, no tuple
    | Variable _ -> failwith("Variable except $1, NYI")
    | String x -> Value.String x
    | Number x -> Value.Number x
    | Float x -> Value.Float x

let variableToFile (target:Variable) (curRow: Value) =
    match target, curRow with
    | (SpecialVariable 1), (File f) -> f
    | _, _ -> failwith("NYI on variableToFile")

let evalBooleanFileFieldAccess field (fileSysInfo:FileSystemInfo) =
    match field with
    | "isDirectory" -> fileSysInfo.Attributes = FileAttributes.Directory
    | "isFile" -> fileSysInfo.Attributes = FileAttributes.Normal
    | _ -> failwith($"unkwon file boolean access: {field}")

let evalFloatFileFieldAccess field (fileSysInfo:FileSystemInfo) =
    match field with
    | "Length" -> (float (fileSysInfo :?>FileInfo).Length)
    | _ -> failwith($"unkwon file boolean access: {field}")


let evalBooleanFieldAccess (target:Value) (fname:String) =
    match target with
    | File f -> evalBooleanFileFieldAccess fname f
    | _ -> failwith($"Boolean field access for unknown target")

let evalExprToFloat (expr:Expr) curRow =
    match expr with
    | Atom (Number x) -> (float x)
    | Atom (Float x) -> x
    | FieldAccess {target=target; field=field} -> variableToFile target curRow |> evalFloatFileFieldAccess field
    | _ -> failwith("NYI in evalExprToFloat")

let evalGt arg1 arg2 curRow =
    let f1 = evalExprToFloat arg1 curRow
    let f2 = evalExprToFloat arg2 curRow
    f1 > f2

let evalBooleanExprWith expr curRow =
    match expr with
    | Atom _ -> failwith("atom is not boolean expr")
    | FieldAccess {target=(SpecialVariable 1); field=fname} -> evalBooleanFieldAccess curRow fname
    | FieldAccess _ -> failwith("NYI field access except for $1")
    | BinOp (GtOp, arg1, arg2) -> evalGt arg1 arg2 curRow
    | BinOp _ -> failwith("NYI binop")


let filter expr valseq =
    // printfn "filter called"
    valseq |> Seq.filter (fun v -> evalBooleanExprWith expr v)

let evalPipedFun pfun valseq =
    match pfun with
    | {Identifier="filter"; Args=arg::[]} -> filter arg valseq
    | {Identifier=x; Args=args} -> failwith($"Unsupported pieped function: {x}")


let pPipeline =
    (sepBy (pfuncall .>> ws) (str_ws "|"))  |>>
        (fun fcalls ->
            // printfn "deb1 %A" fcalls
            match fcalls with
            |head::rest ->
                let {Identifier=fid; Args= args} = head
                let first = evalToplevelFuncall fid args
                // printfn "deb2 %A" rest
                (first, rest) ||> List.fold (fun acc pfun -> evalPipedFun pfun acc)
            |[] -> Seq.empty
        )


test pPipeline """ls("./") | filter($1.isFile)"""

test pfuncall "filter($1.isFile)"

test pPipeline """ls("./")
| filter($1.isFile)
"""



test pfuncall """ls("./")"""


test pPipeline """ls("./")
| filter($1.isFile)
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

