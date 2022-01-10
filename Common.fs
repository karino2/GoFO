module Common

open System
open System.IO

type Value = 
| File of FileSystemInfo 
| Number of int
| Float of float
| Date of DateTime
| Bool of bool
| String of string

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

type Funcall = {Identifier: string; Args: Expr list}
