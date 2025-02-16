open Ast.Ast_types

type expr =
  | Identifier  of string
  | Integer     of int
  | Boolean     of bool
  | UnOp        of un_op * expr
  | BinOp       of bin_op * expr * expr
  | Let         of string * expr
  | If          of expr * block * block
  | Block       of expr list
and
  block = Block of expr list

type function_defn =
  | TFunction of string * param list * block

type program =
  | Program of function_defn list * block
