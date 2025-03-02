open Ast.Ast_types

type expr =
  | Identifier of string
  | Integer of int
  | Boolean of bool
  | StringLiteral of string
  | UnOp of Type.t * un_op * expr
  | BinOp of Type.t * bin_op * expr * expr
  | Let of Type.t * string * expr
  | If of Type.t * expr * block * block
  | Call of Type.t * string * expr list

and block = Block of Type.t * expr list

type function_defn = TFunction of string * param list * Type.t * block
type program = Program of function_defn list * block
