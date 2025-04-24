type name = string
type id = int
type level = int

type ty =
  | TConst of name
  | TApp of ty * ty list
  | TArrow of ty list * ty
  | TVar of tvar ref
  | TList of ty

and tvar = Unbound of id * level | Link of ty | Generic of id

type bin_op =
  | BinOpPlus
  | BinOpMinus
  | BinOpMult
  | BinOpDiv
  | BinOpRem
  | BinOpLessThan
  | BinOpGreaterThan
  | BinOpLessThanEqual
  | BinOpGreaterThanEqual
  | BinOpAnd
  | BinOpOr
  | BinOpEqual
  | BinOpNotEqual

type un_op = UnOpNot | UnOpNegate

let rec string_of_ty = function
  | TConst name -> name
  | TApp (ty, ty_arg_list) ->
      "(" ^ string_of_ty ty ^ " "
      ^ String.concat " " (List.map string_of_ty ty_arg_list)
      ^ ")"
  | TArrow (param_ty_list, return_ty) ->
      "("
      ^ String.concat " -> " (List.map string_of_ty param_ty_list)
      ^ " -> " ^ string_of_ty return_ty ^ ")"
  | TVar { contents = Link ty } -> string_of_ty ty
  | TVar { contents = Generic id } -> "gen_" ^ string_of_int id
  | TVar { contents = Unbound (id, _) } -> "unbound_" ^ string_of_int id
  | TList ty -> "list " ^ string_of_ty ty

let string_of_bin_op = function
  | BinOpPlus -> "+"
  | BinOpMinus -> "-"
  | BinOpMult -> "*"
  | BinOpDiv -> "/"
  | BinOpRem -> "%"
  | BinOpLessThan -> "<"
  | BinOpGreaterThan -> ">"
  | BinOpLessThanEqual -> "<="
  | BinOpGreaterThanEqual -> ">="
  | BinOpAnd -> "&&"
  | BinOpOr -> "||"
  | BinOpEqual -> "=="
  | BinOpNotEqual -> "!="

let string_of_un_op = function UnOpNegate -> "-" | UnOpNot -> "!"
