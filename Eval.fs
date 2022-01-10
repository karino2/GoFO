module Eval 

open Common
open System
open System.IO
open System.Text.RegularExpressions



let booleanFileFieldAccess field (fileSysInfo:FileSystemInfo) =
    match field with
    | "IsDirectory" -> fileSysInfo.Attributes = FileAttributes.Directory
    | "IsFile" -> fileSysInfo.Attributes = FileAttributes.Normal
    | _ -> failwith($"unkwon file boolean access: {field}")

let floatFileFieldAccess field (fileSysInfo:FileSystemInfo) =
    match field with
    | "Length" -> (float (fileSysInfo :?>FileInfo).Length)
    | _ -> failwith($"unkwon file boolean access: {field}")


let special2value (curRow:Row) special =
    let (SpecialVariable i) = special
    rowaccess curRow (i-1)

let variable2file (curRow: Row) (target:Variable) =
    match special2value curRow target with
    | (File f) -> f
    | _ -> failwith("NYI on variable2file")


let booleanFieldAccess (target:Value) (field:String) =
    match target with
    | File f -> booleanFileFieldAccess field f
    | _ -> failwith($"Boolean field access for unknown target")

let toStringTopLevel  = function
| Atom atom ->
    match atom with
    | String x -> x
    | Variable _ -> failwith("Variable to String, NYI")
    | _ -> failwith("Non string value")
| BinOp _ -> failwith("NYI BinOp")
| FieldAccess _ -> failwith("NYI FieldAccess")

let toFloat curRow (expr:Expr) =
    match expr with
    | Atom (Number x) -> (float x)
    | Atom (Float x) -> x
    | FieldAccess {target=target; field=field} -> variable2file curRow target |> floatFileFieldAccess field
    | _ -> failwith("NYI in evalExprToFloat")

let evalGt curRow arg1 arg2  =
    let f1 = toFloat curRow arg1 
    let f2 = toFloat curRow arg2 
    f1 > f2


// only .Name supported
let fieldAccess (target:Value) (field:String) =
    match target, field with
    | File f, "Name" -> Value.String f.Name
    | _ -> failwith($"NYI field access {field}")

let stringFileFieldAccess field target =
    match fieldAccess target field with
    | Value.String x -> x
    | _ -> failwith("Fail to cast field access to string")

let toString curRow (expr:Expr) =
    match expr with
    | Atom (String x) -> x
    | Atom (Variable x) ->
        match special2value curRow x with
        | Value.String y -> y
        | _ -> failwith("NYI, unknown variable to String")
    | FieldAccess {target=target; field=field} -> special2value curRow target |> stringFileFieldAccess field
    | _ -> failwith("NYI in toString")

// arg1 ~ arg2
let evalMatch curRow arg1 arg2  =
    let target = toString curRow arg1 
    let pat = toString curRow arg2 
    Regex(pat).Match(target).Success


let toBoolean (curRow:Row) expr =
    match expr with
    | Atom _ -> failwith("atom is not boolean expr")
    | FieldAccess {target=sv; field=fname} -> booleanFieldAccess (special2value curRow sv) fname
    | BinOp (GtOp, arg1, arg2) -> evalGt curRow arg1 arg2
    | BinOp (RegMatchOp, arg1, arg2) -> evalMatch curRow arg1 arg2
    | BinOp _ -> failwith("NYI binop")

let atom2value (atom:Atom) =
    match atom with
    | String x -> Value.String x
    | Number x -> Value.Number x
    | Float x -> Value.Float x 
    | Variable x -> failwith ("variable at atom2value, NYI")


let toValue (curRow:Row) expr =
    match expr with
    | Atom atom -> atom2value atom
    | FieldAccess {target=sv; field=fname} -> fieldAccess (special2value curRow sv) fname
    | BinOp _ -> failwith("NYI binop toValue")
