type name = string

type id = int
type level = int

type ty =
  | TConst of name
  | TApp of ty * ty list
  | TArrow of ty list * ty
  | TVar of tvar ref
  | TList of ty
  | TRecord of (name * ty) list

and tvar =
  | Unbound of id * level
  | Link of ty
  | Generic of id

type expr =
  | Identifier of name
  | Call of expr * expr list
  | Fun of (name * ty) list * expr * ty
  (* | Fun of name list * expr *)
  | Let of name * expr * expr
  | If of expr * expr * expr
  | BinOp of name * expr * expr
  | UnOp of name * expr
  | StringLiteral of string
  | Integer of int
  | Boolean of bool
  | Unit of unit
  | List of expr list
  | Record of (name * expr) list
