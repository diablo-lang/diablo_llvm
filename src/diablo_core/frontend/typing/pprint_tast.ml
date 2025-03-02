open Ast.Ast_types
open Typed_ast

let pprint_type_env env =
  let bindings = List.map (fun (x, t) -> Printf.sprintf "%s: %s" x (Type.to_string t)) env in
  String.concat ", " bindings

let pprint_type t =
  Type.to_string t

let rec pprint_program program =
  match program with
  | Program (fs, exprs) -> Printf.sprintf "%s %s" (String.concat "\n" (List.map pprint_function_defn fs)) (pprint_block exprs)

and pprint_function_defn function_defn =
  match function_defn with
  | TFunction (name, params, return_type, body) ->
    Printf.sprintf "function %s(%s): %s = %s"
      name
      (String.concat ", " (List.map pprint_param params))
      (pprint_type return_type)
      (pprint_block body)
and
  pprint_param param =
    match param with
    | TParam (t, s) -> Printf.sprintf "%s %s" (pprint_type t) s

and pprint_block block =
  match block with
  | Block (t, exprs) -> Printf.sprintf "%s %s" (pprint_type t) (String.concat " " (List.map pprint_expr exprs))

and pprint_expr expr =
  match expr with
  | Identifier s -> s
  | Integer i -> string_of_int i
  | Boolean b -> string_of_bool b
  | StringLiteral s -> Printf.sprintf "\"%s\"" s
  | UnOp (t, op, e) -> Printf.sprintf "(%s %s %s)" (pprint_type t) (pprint_un_op op) (pprint_expr e)
  | BinOp (t, op, e1, e2) -> Printf.sprintf "(%s %s %s %s)" (pprint_type t) (pprint_bin_op op) (pprint_expr e1) (pprint_expr e2)
  | Let (t, x, e) -> Printf.sprintf "(%s %s = %s)" (pprint_type t) x (pprint_expr e)
  | If (t, cond, then_branch, else_branch) -> Printf.sprintf "(%s if %s then %s else %s)" (pprint_type t) (pprint_expr cond) (pprint_block then_branch) (pprint_block else_branch)
  | Call (t, fn, args) -> Printf.sprintf "(%s %s(%s))" (pprint_type t) fn (String.concat ", " (List.map pprint_expr args))

and pprint_bin_op op =
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

and pprint_un_op op =
  match op with
  | UnOpNot -> "!"
  | UnOpNegate -> "-"