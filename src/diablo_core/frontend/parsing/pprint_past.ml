open Ast.Ast_types
open Parsed_ast

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

let pprint_un_op op = match op with UnOpNot -> "!" | UnOpNegate -> "-"

let rec pprint_expr expr =
  match expr with
  | Identifier s -> s
  | Integer i -> string_of_int i
  | Boolean b -> string_of_bool b
  | StringLiteral s -> Printf.sprintf "\"%s\"" s
  | UnOp (op, e) -> Printf.sprintf "(%s %s)" (pprint_un_op op) (pprint_expr e)
  | BinOp (op, e1, e2) ->
      Printf.sprintf "(%s %s %s)" (pprint_expr e1) (pprint_bin_op op)
        (pprint_expr e2)
  | Let (s, e) -> Printf.sprintf "let %s = %s" s (pprint_expr e)
  | If (e, then_branch, else_branch) ->
      Printf.sprintf "if %s then %s else %s" (pprint_expr e)
        (pprint_block then_branch) (pprint_block else_branch)
  | Call (s, es) ->
      Printf.sprintf "%s(%s)" s (String.concat ", " (List.map pprint_expr es))
  | ExternCall (s, es) ->
      Printf.sprintf "extern %s(%s)" s
        (String.concat ", " (List.map pprint_expr es))

and pprint_block block =
  match block with
  | Block exprs -> String.concat "\n" (List.map pprint_expr exprs)

let rec pprint_function_defn function_defn =
  match function_defn with
  | TFunction (name, params, return_type, body) ->
      Printf.sprintf "function %s(%s): %s = %s" name
        (String.concat ", " (List.map pprint_param params))
        (Type.to_string return_type)
        (pprint_block body)

and pprint_param param =
  match param with
  | TParam (t, s) -> Printf.sprintf "%s %s" (Type.to_string t) s

let pprint_program program =
  match program with
  | Program (_import_stms, fs, exprs) ->
      Printf.sprintf "%s %s"
        (String.concat "\n" (List.map pprint_function_defn fs))
        (pprint_block exprs)
