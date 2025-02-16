open Ast.Ast_types

type expr =
  | Identifier  of string
  | Integer     of int
  | Boolean     of bool
  | UnOp        of diablo_type *  un_op * expr
  | BinOp       of diablo_type *  bin_op * expr * expr
  | Let         of diablo_type * string * expr
  | If          of diablo_type * expr * block * block
  | Block       of expr list
and
  block = Block of diablo_type * expr list

type function_defn =
  | TFunction of string * diablo_type * param list * block

type program =
  | Program of function_defn list * block
