open Ast.Ast_types

type expr =
  | Identifier  of string
  | Integer     of int
  | Boolean     of bool
  | UnOp        of Type.t *  un_op * expr
  | BinOp       of Type.t *  bin_op * expr * expr
  | Let         of Type.t * string * expr
  | If          of Type.t * expr * expr * expr

type function_defn =
  | TFunction of string * param list * Type.t * expr

type program =
  | Program of function_defn list * expr
