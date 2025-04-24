open Ast.Ast_types

type expr =
  | Identifier of name
  | Integer of int
  | Boolean of bool
  | StringLiteral of string
  | Unit
  | Call of expr * expr list
  | Lambda of (name * ty) list * expr * ty
  | LetIn of name * expr * expr
  | If of expr * expr * expr
  | BinOp of bin_op * expr * expr
  | UnOp of un_op * expr
  | List of expr list

type top_level_declaration =
  | Function of name * (name * ty) list * expr * ty
  | Let of name * expr

type import_stmt = Import of string
type module_defn = ModuleDefinition of name * top_level_declaration list
type module_file = Module of import_stmt list * module_defn
type program = Program of import_stmt list * top_level_declaration list
