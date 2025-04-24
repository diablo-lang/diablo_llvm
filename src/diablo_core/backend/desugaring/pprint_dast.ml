open Ast.Ast_types
open Desugared_ast

let rec string_of_expr = function
  | Identifier (name, ty) -> name ^ " : " ^ string_of_ty ty
  | Integer n -> string_of_int n
  | Boolean b -> string_of_bool b
  | StringLiteral s -> "\"" ^ s ^ "\""
  | Unit -> "()"
  | Call (f, args, ty) ->
      "Call(" ^ string_of_expr f ^ ", ["
      ^ String.concat ", " (List.map string_of_expr args)
      ^ "], " ^ string_of_ty ty ^ ")"
  | Lambda (params, body, ty) ->
      "Lambda(["
      ^ String.concat ", "
          (List.map (fun (p, t) -> p ^ ": " ^ string_of_ty t) params)
      ^ "], " ^ string_of_expr body ^ ", " ^ string_of_ty ty ^ ")"
  | LetIn (name, value, body, ty) ->
      "LetIn(" ^ name ^ ", " ^ string_of_expr value ^ ",\n  "
      ^ string_of_expr body ^ ", " ^ string_of_ty ty ^ ")"
  | If (cond, then_expr, else_expr, ty) ->
      "If(" ^ string_of_expr cond ^ ", " ^ string_of_expr then_expr ^ ", "
      ^ string_of_expr else_expr ^ ", " ^ string_of_ty ty ^ ")"
  | BinOp (op, left, right, ty) ->
      "BinOp(" ^ string_of_bin_op op ^ ", " ^ string_of_expr left ^ ", "
      ^ string_of_expr right ^ ", " ^ string_of_ty ty ^ ")"
  | UnOp (op, expr, ty) ->
      "UnOp(" ^ string_of_un_op op ^ ", " ^ string_of_expr expr ^ ", "
      ^ string_of_ty ty ^ ")"
  | List (elements, ty) ->
      "List(["
      ^ String.concat ", " (List.map string_of_expr elements)
      ^ "], " ^ string_of_ty ty ^ ")"

and string_of_top_level_declaration = function
  | Let (name, value, ty) ->
      "Let(" ^ name ^ ", " ^ string_of_expr value ^ ", " ^ string_of_ty ty ^ ")"

and string_of_program (Program (declarations, ty)) =
  "Program([\n" ^ "[\n"
  ^ String.concat "\n" (List.map string_of_top_level_declaration declarations)
  ^ "\n], " ^ string_of_ty ty ^ ")"
