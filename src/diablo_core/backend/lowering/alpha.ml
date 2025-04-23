open Desugaring
open Fresh

module Alpha = struct
  module SM = Map.Make(String)
  let rename =
    let rec rename_expr env : Desugared_ast.expr -> Desugared_ast.expr = function
      | Desugared_ast.Identifier (name, ty) ->
          (match SM.find_opt name env with
            | Some name' -> Desugared_ast.Identifier(name', ty)
            | _ -> failwith ("Scope checking failed: undefined variable " ^ name))
      | Desugared_ast.Integer i -> Integer i
      | Desugared_ast.Boolean b -> Boolean b
      | Desugared_ast.StringLiteral s -> StringLiteral s
      | Desugared_ast.Unit -> Unit
      | Desugared_ast.Call (callee, args, ty) ->
          Call (rename_expr env callee, List.map (rename_expr env) args, ty)
      | Desugared_ast.Lambda (params, body, ty) ->
          let params' = List.map (fun (name, ty) -> (NameGen.fresh name, ty)) params in
          let env' = List.fold_left2 (fun env (orig_name, _) (fresh_name, _) ->
            SM.add orig_name fresh_name env
          ) env params params' in
          let body' = rename_expr env' body in
          Lambda (params', body', ty)
      | Desugared_ast.LetIn (name, expr, expr', ty) ->
          let name' = NameGen.fresh name in
          let env' = SM.add name name' env in
          LetIn (name', rename_expr env expr, rename_expr env' expr', ty)
      | Desugared_ast.If (cond_expr, then_expr, else_expr, ty) ->
        If (rename_expr env cond_expr, rename_expr env then_expr, rename_expr env else_expr, ty)
      | Desugared_ast.BinOp (op, lhs_expr, rhs_expr, ty) ->
          BinOp (op, rename_expr env lhs_expr, rename_expr env rhs_expr, ty)
      | Desugared_ast.UnOp (op, expr, ty) ->
          UnOp (op, rename_expr env expr, ty)
      | Desugared_ast.List (expr_list, ty) ->
          List (List.map (rename_expr env) expr_list, ty)

    in

    let rename_top_level_declaration env = function
    | Desugared_ast.Let (name, expr, ty) ->
        let name' = NameGen.fresh name in
        let env' = SM.add name name' env in
        (env', Desugared_ast.Let (name', rename_expr env' expr, ty))

    in

    let rename_program : Desugared_ast.program -> Desugared_ast.program = function
    | Program (top_level_declarations, ty) ->
        let _, decls' =
          List.fold_left
            (fun (env, acc) decl ->
              let env', decl' = rename_top_level_declaration env decl in
              (env', acc @ [decl']))
            (SM.empty, [])
            top_level_declarations
        in
        Program (decls', ty)

    in

    rename_program
end
