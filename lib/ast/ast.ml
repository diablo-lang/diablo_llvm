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

type un_op =
  | UnOpNot
  | UnOpNegate

type expr =
  | Identifier of string
  | Integer of int
  | Boolean of bool
  | UnOp of un_op * expr
  | BinOp of bin_op * expr * expr
  | Let of string * expr
  | If of expr * block * block
  | Block of expr list
  | Function of string * block
and
  block = Block of expr list

type param =
  | Param of string

type function_defn =
  | Function of string * param list * block

type program = Program of function_defn list * block

let pprint_bin_op op =
  match op with
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

let pprint_un_op op =
  match op with
  | UnOpNot -> "!"
  | UnOpNegate -> "-"

let rec pprint_expr expr =
  match expr with
  | Identifier s -> s
  | Integer i -> string_of_int i
  | Boolean b -> string_of_bool b
  | UnOp (op, e) -> Printf.sprintf "(%s %s)" (pprint_un_op op) (pprint_expr e)
  | BinOp (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)" (pprint_expr e1) (pprint_bin_op op) (pprint_expr e2)
  | Let (s, e) -> Printf.sprintf "let %s = %s" s (pprint_expr e)
  | If (e, b1, b2) ->
    Printf.sprintf "if %s then %s else %s" (pprint_expr e) (pprint_block b1) (pprint_block b2)
  | Block es -> Printf.sprintf "[%s]" (String.concat ", " (List.map pprint_expr es))
  | Function (s, b) -> Printf.sprintf "function %s %s" s (pprint_block b)

and
  pprint_block block =
    match block with
    | Block es -> Printf.sprintf "[%s]" (String.concat ", " (List.map pprint_expr es))

let rec pprint_function_defn function_defn =
  match function_defn with
  | Function (s, ps, b) ->  Printf.sprintf "function %s(%s) %s" s (String.concat ", " (List.map pprint_param ps)) (pprint_block b)
and
  pprint_param param =
    match param with
    | Param s -> s

let pprint_program program =
  match program with
  | Program (fs, b) -> Printf.sprintf "%s %s" (String.concat "\n" (List.map pprint_function_defn fs)) (pprint_block b)
