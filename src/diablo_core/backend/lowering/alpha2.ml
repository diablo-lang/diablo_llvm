open Desugaring.Desugared_ast
open Fresh

module StringMap = Map.Make(String)

let rec alpha_expr (env : string StringMap.t) (e : expr) : expr =
  match e with
  | Identifier (name, ty) ->
      let new_name = StringMap.find_opt name env |> Option.value ~default:name in
      Identifier (new_name, ty)

  | Integer _ | Boolean _ | StringLiteral _ | Unit -> e

  | Call (fn, args, ty) ->
      let fn' = alpha_expr env fn in
      let args' = List.map (alpha_expr env) args in
      Call (fn', args', ty)

  | Lambda (params, body, ty) ->
      let (env', new_params) = extend_env_with_params env params in
      let body' = alpha_expr env' body in
      Lambda (new_params, body', ty)

  | LetIn (name, value, body, ty) ->
      let value' = alpha_expr env value in
      let new_name = NameGen.fresh name in
      let env' = StringMap.add name new_name env in
      let body' = alpha_expr env' body in
      LetIn (new_name, value', body', ty)

  | If (cond, then_e, else_e, ty) ->
      let cond' = alpha_expr env cond in
      let then' = alpha_expr env then_e in
      let else' = alpha_expr env else_e in
      If (cond', then', else', ty)

  | BinOp (op, lhs, rhs, ty) ->
      let lhs' = alpha_expr env lhs in
      let rhs' = alpha_expr env rhs in
      BinOp (op, lhs', rhs', ty)

  | UnOp (op, e, ty) ->
      let e' = alpha_expr env e in
      UnOp (op, e', ty)

  | List (items, ty) ->
      let items' = List.map (alpha_expr env) items in
      List (items', ty)

and extend_env_with_params env params =
  List.fold_left
    (fun (env_acc, rev_params) (name, ty) ->
      let new_name = NameGen.fresh name in
      (StringMap.add name new_name env_acc, (new_name, ty) :: rev_params)
    )
    (env, [])
    params
  |> fun (env', rev_params) -> (env', List.rev rev_params)

let alpha_top (decl : top_level_declaration) : top_level_declaration =
  match decl with
  | Let (name, expr, ty) ->
      let new_name = NameGen.fresh name in
      let env = StringMap.singleton name new_name in
      Let (new_name, alpha_expr env expr, ty)

let alpha_program (Program (decls, ty)) : program =
  Program (List.map alpha_top decls, ty)
  