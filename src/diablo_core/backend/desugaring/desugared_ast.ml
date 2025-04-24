open Ast.Ast_types

type expr =
  | Identifier of name * ty
  | Integer of int
  | Boolean of bool
  | StringLiteral of string
  | Unit
  | Call of expr * expr list * ty
  | Lambda of (name * ty) list * expr * ty
  | LetIn of name * expr * expr * ty
  | If of expr * expr * expr * ty
  | BinOp of bin_op * expr * expr * ty
  | UnOp of un_op * expr * ty
  | List of expr list * ty

type top_level_declaration = Let of name * expr * ty
type program = Program of top_level_declaration list * ty
