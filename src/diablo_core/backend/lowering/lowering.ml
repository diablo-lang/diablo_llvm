open Desugaring
open Ast.Ast_types

let rec lower_program program =
  let (Desugared_ast.Program (imports, functions, main)) = program in
  let lowered_imports = List.map desugar_import imports in
  let lowered_functions = List.map desugar_function functions in
  let lowered_main = desugar_block main in
  let lowered_program =
    Lowered_ast.Program (lowered_imports, lowered_functions, lowered_main)
  in
  lowered_program

and desugar_import (Desugared_ast.Import module_name) = Lowered_ast.Import module_name

and desugar_function (Desugared_ast.TFunction (name, params, return_type, block)) =
  let lowered_params = List.map desugar_param params in
  let lowered_block = desugar_block block in
  Lowered_ast.TFunction (name, lowered_params, return_type, lowered_block)

and desugar_block (Desugared_ast.Block (return_type, exprs)) =
  let lowered_exprs = List.map desugar_expr exprs in
  Lowered_ast.Block (return_type, lowered_exprs)

and desugar_expr expr =
  match expr with
  | Desugared_ast.Identifier s -> Lowered_ast.Identifier s
  | Desugared_ast.Integer i -> Lowered_ast.Integer i
  | Desugared_ast.Boolean b -> Lowered_ast.Boolean b
  | Desugared_ast.StringLiteral s -> Lowered_ast.StringLiteral s
  | Desugared_ast.UnOp (t, op, e) -> Lowered_ast.UnOp (t, op, desugar_expr e)
  | Desugared_ast.BinOp (t, op, e1, e2) ->
      Lowered_ast.BinOp (t, op, desugar_expr e1, desugar_expr e2)
  | Desugared_ast.Let (t, s, e) -> Lowered_ast.Let (t, s, desugar_expr e)
  | Desugared_ast.If (t, e1, e2, e3) ->
      Lowered_ast.If (t, desugar_expr e1, desugar_block e2, desugar_block e3)
  | Desugared_ast.Call (t, s, es) ->
      Lowered_ast.Call (t, s, List.map desugar_expr es)
  | Desugared_ast.ExternCall (t, s, es) ->
      Lowered_ast.ExternCall (t, s, List.map desugar_expr es)

and desugar_param (TParam (t, s)) = TParam (t, s)

let desugar_module (Desugared_ast.Module (name, functions)) =
  let lowered_functions = List.map desugar_function functions in
  Lowered_ast.Module (name, lowered_functions)
