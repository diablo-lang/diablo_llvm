open Ast.Ast_types

type expr =
  | Identifier of string
  | Integer of int
  | Boolean of bool
  | StringLiteral of string
  | UnOp of un_op * expr
  | BinOp of bin_op * expr * expr
  | Let of string * expr
  | If of expr * block * block
  | Call of string * expr list
  | ExternCall of string * expr list

and block = Block of expr list

type function_defn = TFunction of string * param list * Type.t * block
type module_defn = Module of string * function_defn list
type import_stmt = Import of string
type program = Program of import_stmt list * function_defn list * block
