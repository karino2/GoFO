module Eval 

open Common
open System
open System.IO


let atom2value (atom:Atom) (curRow:Value) =
    match atom with
    | Variable (SpecialVariable 1) -> curRow // currently, no tuple
    | Variable _ -> failwith("Variable except $1, NYI")
    | String x -> Value.String x
    | Number x -> Value.Number x
    | Float x -> Value.Float x

let variable2file (target:Variable) (curRow: Value) =
    match target, curRow with
    | (SpecialVariable 1), (File f) -> f
    | _, _ -> failwith("NYI on variableToFile")

let booleanFileFieldAccess field (fileSysInfo:FileSystemInfo) =
    match field with
    | "isDirectory" -> fileSysInfo.Attributes = FileAttributes.Directory
    | "isFile" -> fileSysInfo.Attributes = FileAttributes.Normal
    | _ -> failwith($"unkwon file boolean access: {field}")

let floatFileFieldAccess field (fileSysInfo:FileSystemInfo) =
    match field with
    | "Length" -> (float (fileSysInfo :?>FileInfo).Length)
    | _ -> failwith($"unkwon file boolean access: {field}")


let booleanFieldAccess (target:Value) (fname:String) =
    match target with
    | File f -> booleanFileFieldAccess fname f
    | _ -> failwith($"Boolean field access for unknown target")

let toString  = function
| Atom atom ->
    match atom with
    | String x -> x
    | Variable _ -> failwith("Variable to String, NYI")
    | _ -> failwith("Non string value")
| BinOp _ -> failwith("NYI BinOp")
| FieldAccess _ -> failwith("NYI FieldAccess")

let toFloat (expr:Expr) curRow =
    match expr with
    | Atom (Number x) -> (float x)
    | Atom (Float x) -> x
    | FieldAccess {target=target; field=field} -> variable2file target curRow |> floatFileFieldAccess field
    | _ -> failwith("NYI in evalExprToFloat")

let evalGt arg1 arg2 curRow =
    let f1 = toFloat arg1 curRow
    let f2 = toFloat arg2 curRow
    f1 > f2

let toBoolean expr curRow =
    match expr with
    | Atom _ -> failwith("atom is not boolean expr")
    | FieldAccess {target=(SpecialVariable 1); field=fname} -> booleanFieldAccess curRow fname
    | FieldAccess _ -> failwith("NYI field access except for $1")
    | BinOp (GtOp, arg1, arg2) -> evalGt arg1 arg2 curRow
    | BinOp _ -> failwith("NYI binop")

