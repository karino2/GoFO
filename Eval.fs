module Eval 

open Common
open System
open System.IO



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

let toBoolean (curRow:Row) expr =
    match expr with
    | Atom _ -> failwith("atom is not boolean expr")
    | FieldAccess {target=sv; field=fname} -> booleanFieldAccess (special2value curRow sv) fname
    | BinOp (GtOp, arg1, arg2) -> evalGt curRow arg1 arg2
    | BinOp _ -> failwith("NYI binop")

