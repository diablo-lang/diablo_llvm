open Ast.Ast_types
open Parsed_ast

let rec string_of_expr = function
  | Identifier name -> name
  | Integer n -> string_of_int n
  | Boolean b -> string_of_bool b
  | StringLiteral s -> "\"" ^ s ^ "\""
  | Unit -> "()"
  | Call (f, args) -> 
      "Call(" ^ string_of_expr f ^ ", [" ^ String.concat ", " (List.map string_of_expr args) ^ "])"
  | Lambda (params, body, ty) ->
      "Lambda([" ^ String.concat ", " (List.map (fun (p, t) -> p ^ ": " ^ string_of_ty t) params) ^ "], " ^ string_of_expr body ^ ", " ^ string_of_ty ty ^ ")"
  | LetIn (name, value, body) ->
      "LetIn(" ^ name ^ ", " ^ string_of_expr value ^ ",\n  " ^ string_of_expr body ^ ")"
  | If (cond, then_expr, else_expr) ->
      "If(" ^ string_of_expr cond ^ ", " ^ string_of_expr then_expr ^ ", " ^ string_of_expr else_expr ^ ")"
  | BinOp (op, left, right) ->
      "BinOp(" ^ string_of_bin_op op ^ ", " ^ string_of_expr left ^ ", " ^ string_of_expr right ^ ")"
  | UnOp (op, expr) ->
      "UnOp(" ^ string_of_un_op op ^ ", " ^ string_of_expr expr ^ ")"
  | List elements ->
      "List([" ^ String.concat ", " (List.map string_of_expr elements) ^ "])"

and string_of_top_level_declaration = function
  | Function (name, params, body, ty) ->
      "Function(" ^ name ^ ", [" ^ String.concat ", " (List.map (fun (p, t) -> p ^ ": " ^ string_of_ty t) params) ^ "],\n  " ^ string_of_expr body ^ ", " ^ string_of_ty ty ^ ")"
  | Let (name, value) ->
      "Let(" ^ name ^ ", " ^ string_of_expr value ^ ")"

and string_of_import_stmt = function
  | Import module_name -> "Import(" ^ module_name ^ ")"

and string_of_module_file = function
  | Module (imports, module_defn) ->
      "Module([\n" ^ String.concat "\n" (List.map string_of_import_stmt imports) ^ "\n], [\n" ^ (string_of_module_defn module_defn) ^ "\n])"

and string_of_module_defn = function
    | ModuleDefinition (name, decls) ->
        "ModuleDefinition(" ^ name ^ ", [\n" ^ String.concat "\n" (List.map string_of_top_level_declaration decls) ^ "\n])"

and string_of_program (Program (imports, declarations)) =
    "Program([\n" ^ String.concat "\n" (List.map string_of_import_stmt imports) ^ "\n], [\n" ^ String.concat "\n" (List.map string_of_top_level_declaration declarations) ^ "\n])"