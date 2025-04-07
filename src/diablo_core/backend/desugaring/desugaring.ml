open Typing
open Ast.Ast_types

let rec desugar_program program =
  let (Typed_ast.Program (imports, functions, main)) = program in
  let desugared_imports = List.map desugar_import imports in
  let desugared_functions = List.map desugar_function functions in
  let desugared_main = desugar_block main in
  let desugared_program =
    Desugared_ast.Program (desugared_imports, desugared_functions, desugared_main)
  in
  desugared_program

and desugar_import (Typed_ast.Import module_name) = Desugared_ast.Import module_name

and desugar_function (Typed_ast.TFunction (name, params, return_type, block)) =
  let desugared_params = List.map desugar_param params in
  let desugared_block = desugar_block block in
  Desugared_ast.TFunction (name, desugared_params, return_type, desugared_block)

and desugar_block (Typed_ast.Block (return_type, exprs)) =
  let desugared_exprs = List.map desugar_expr exprs in
  Desugared_ast.Block (return_type, desugared_exprs)

and desugar_expr expr =
  match expr with
  | Typed_ast.Identifier s -> Desugared_ast.Identifier s
  | Typed_ast.Integer i -> Desugared_ast.Integer i
  | Typed_ast.Boolean b -> Desugared_ast.Boolean b
  | Typed_ast.StringLiteral s -> Desugared_ast.StringLiteral s
  | Typed_ast.UnOp (t, op, e) -> Desugared_ast.UnOp (t, op, desugar_expr e)
  | Typed_ast.BinOp (t, op, e1, e2) ->
      Desugared_ast.BinOp (t, op, desugar_expr e1, desugar_expr e2)
  | Typed_ast.Let (t, s, e) -> Desugared_ast.Let (t, s, desugar_expr e)
  | Typed_ast.If (t, e1, e2, e3) ->
      Desugared_ast.If (t, desugar_expr e1, desugar_block e2, desugar_block e3)
  | Typed_ast.Call (t, s, es) ->
      Desugared_ast.Call (t, s, List.map desugar_expr es)
  | Typed_ast.ExternCall (t, s, es) ->
      Desugared_ast.ExternCall (t, s, List.map desugar_expr es)

and desugar_param (TParam (t, s)) = TParam (t, s)

let desugar_module (Typed_ast.Module (name, functions)) =
  let desugared_functions = List.map desugar_function functions in
  Desugared_ast.Module (name, desugared_functions)
