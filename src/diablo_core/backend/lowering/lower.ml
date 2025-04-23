open Desugaring

let rec lower_expr = function
  | Desugared_ast.Identifier (name, ty) -> Lowered_ast.Identifier (name, ty)
  | Desugared_ast.Integer i -> Lowered_ast.Integer i
  | Desugared_ast.Boolean b -> Lowered_ast.Boolean b
  | Desugared_ast.StringLiteral s -> Lowered_ast.StringLiteral s
  | Desugared_ast.Unit -> Lowered_ast.Unit
  | Desugared_ast.Call (expr, expr_list, ty) ->
      Lowered_ast.Call (lower_expr expr, List.map lower_expr expr_list, ty)
  | Desugared_ast.Lambda (params, expr, ty) ->
      Lowered_ast.Lambda (params, lower_expr expr, ty)
  | Desugared_ast.LetIn (name, expr, expr', ty) ->
      Lowered_ast.LetIn (name, lower_expr expr, lower_expr expr', ty)
  | Desugared_ast.If (expr, expr', expr'', ty) ->
      Lowered_ast.If (lower_expr expr, lower_expr expr', lower_expr expr'', ty)
  | Desugared_ast.BinOp (op, expr, expr', ty) ->
      Lowered_ast.BinOp (op, lower_expr expr, lower_expr expr', ty)
  | Desugared_ast.UnOp (op, expr, ty) ->
      Lowered_ast.UnOp (op, lower_expr expr, ty)
  | Desugared_ast.List (expr_list, ty) ->
      Lowered_ast.List (List.map lower_expr expr_list, ty)

let lower_top_level_declaration = function
  | Desugared_ast.Let (name, expr, ty) ->
    Lowered_ast.Let (name, lower_expr expr, ty)

let lower_program (program : Desugared_ast.program) =
  match program with
  | Desugared_ast.Program (top_level_declarations, program_ty) ->
      Lowered_ast.Program
        ( List.map lower_top_level_declaration top_level_declarations,
          program_ty )
