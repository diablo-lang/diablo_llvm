open Typing

let rec desugar_expr = function
  | Typed_ast.Identifier (name, ty) -> Desugared_ast.Identifier (name, ty)
  | Typed_ast.Integer i -> Desugared_ast.Integer i
  | Typed_ast.Boolean b -> Desugared_ast.Boolean b
  | Typed_ast.StringLiteral s -> Desugared_ast.StringLiteral s
  | Typed_ast.Unit -> Desugared_ast.Unit
  | Typed_ast.Call (expr, expr_list, ty) ->
      Desugared_ast.Call (desugar_expr expr, List.map desugar_expr expr_list, ty)
  | Typed_ast.Lambda (params, expr, ty) ->
      Desugared_ast.Lambda (params, desugar_expr expr, ty)
  | Typed_ast.LetIn (name, expr, expr', ty) ->
      Desugared_ast.LetIn (name, desugar_expr expr, desugar_expr expr', ty)
  | Typed_ast.If (expr, expr', expr'', ty) ->
      Desugared_ast.If
        (desugar_expr expr, desugar_expr expr', desugar_expr expr'', ty)
  | Typed_ast.BinOp (op, expr, expr', ty) ->
      Desugared_ast.BinOp (op, desugar_expr expr, desugar_expr expr', ty)
  | Typed_ast.UnOp (op, expr, ty) ->
      Desugared_ast.UnOp (op, desugar_expr expr, ty)
  | Typed_ast.List (expr_list, ty) ->
      Desugared_ast.List (List.map desugar_expr expr_list, ty)

let desguar_top_level_declaration = function
  | Typed_ast.Function (name, params, expr, ty) ->
      let lambda = Typed_ast.Lambda (params, expr, ty) in
      Desugared_ast.Let (name, desugar_expr lambda, ty)
  | Typed_ast.Let (name, expr, ty) ->
      Desugared_ast.Let (name, desugar_expr expr, ty)

let desugar_program (program : Typed_ast.program) =
  match program with
  | Typed_ast.Program (top_level_declarations, program_ty) ->
      Desugared_ast.Program
        ( List.map desguar_top_level_declaration top_level_declarations,
          program_ty )
