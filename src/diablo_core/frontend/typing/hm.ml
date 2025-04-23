open Ast.Ast_types
open Parsing

let rec string_of_ty = function
  | TConst name -> name
  | TApp (ty, ty_arg_list) ->
      "(" ^ string_of_ty ty ^ " "
      ^ String.concat " " (List.map string_of_ty ty_arg_list)
      ^ ")"
  | TArrow (param_ty_list, return_ty) ->
      "("
      ^ String.concat " -> " (List.map string_of_ty param_ty_list)
      ^ " -> " ^ string_of_ty return_ty ^ ")"
  | TVar { contents = Link ty } -> string_of_ty ty
  | TVar { contents = Generic id } -> "gen_" ^ string_of_int id
  | TVar { contents = Unbound (id, _) } -> "unbound_" ^ string_of_int id
  | TList ty -> "list " ^ string_of_ty ty

let current_id = ref 0

let next_id () =
  let id = !current_id in
  current_id := id + 1;
  id

let reset_id () = current_id := 0
let new_var level = TVar (ref (Unbound (next_id (), level)))
let new_gen_var () = TVar (ref (Generic (next_id ())))

exception TypeError of string

let error msg = raise (TypeError msg)

module Env = struct
  module StringMap = Map.Make (String)

  type env = ty StringMap.t

  let empty = StringMap.empty
  let extend env name ty = StringMap.add name ty env
  let lookup env name = StringMap.find name env

  let string_of_env env =
    StringMap.fold
      (fun name ty acc -> name ^ ": " ^ string_of_ty ty ^ "\n" ^ acc)
      env ""
end

let occurs_check_adjust_levels tvar_id tvar_level ty =
  let rec f = function
    | TVar { contents = Link ty } -> f ty
    | TVar { contents = Generic _ } -> assert false
    | TVar ({ contents = Unbound (other_id, other_level) } as other_tvar) ->
        if other_id = tvar_id then error "recursive types"
        else if other_level > tvar_level then
          other_tvar := Unbound (other_id, tvar_level)
        else ()
    | TApp (ty, ty_arg_list) ->
        f ty;
        List.iter f ty_arg_list
    | TArrow (param_ty_list, return_ty) ->
        List.iter f param_ty_list;
        f return_ty
    | TConst _ -> ()
    | TList ty -> f ty
  in
  f ty

let rec unify ty1 ty2 =
  if ty1 == ty2 then ()
  else
    match (ty1, ty2) with
    | TConst name1, TConst name2 when name1 = name2 -> ()
    | TApp (ty1, ty_arg_list1), TApp (ty2, ty_arg_list2) ->
        unify ty1 ty2;
        List.iter2 unify ty_arg_list1 ty_arg_list2
    | TArrow (param_ty_list1, return_ty1), TArrow (param_ty_list2, return_ty2)
      ->
        List.iter2 unify param_ty_list1 param_ty_list2;
        unify return_ty1 return_ty2
    | TVar { contents = Link ty1 }, ty2 | ty1, TVar { contents = Link ty2 } ->
        unify ty1 ty2
    | TVar { contents = Unbound (id1, _) }, TVar { contents = Unbound (id2, _) }
      when id1 = id2 ->
        assert false
    | TVar ({ contents = Unbound (id, level) } as tvar), ty
    | ty, TVar ({ contents = Unbound (id, level) } as tvar) ->
        occurs_check_adjust_levels id level ty;
        tvar := Link ty
    | _, _ -> error "Cannot unify types"

let rec generalize level = function
  | TVar { contents = Unbound (id, other_level) } when other_level > level ->
      TVar (ref (Generic id))
  | TApp (ty, ty_arg_list) ->
      TApp (generalize level ty, List.map (generalize level) ty_arg_list)
  | TArrow (param_ty_list, return_ty) ->
      TArrow
        (List.map (generalize level) param_ty_list, generalize level return_ty)
  | TVar { contents = Link ty } -> generalize level ty
  | (TVar { contents = Generic _ } | TVar { contents = Unbound _ } | TConst _)
    as ty ->
      ty
  | TList ty -> TList (generalize level ty)

let instantiate level ty =
  let id_var_map = Hashtbl.create 10 in
  let rec f ty =
    match ty with
    | TConst _ -> ty
    | TVar { contents = Link ty } -> f ty
    | TVar { contents = Generic id } -> (
        try Hashtbl.find id_var_map id
        with Not_found ->
          let var = new_var level in
          Hashtbl.add id_var_map id var;
          var)
    | TVar { contents = Unbound _ } -> ty
    | TApp (ty, ty_arg_list) -> TApp (f ty, List.map f ty_arg_list)
    | TArrow (param_ty_list, return_ty) ->
        TArrow (List.map f param_ty_list, f return_ty)
    | TList ty -> TList (f ty)
  in
  f ty

let rec match_fun_ty num_params = function
  | TArrow (param_ty_list, return_ty) ->
      if List.length param_ty_list <> num_params then
        error
          ("Expected " ^ string_of_int num_params ^ " arguments, got "
          ^ string_of_int (List.length param_ty_list))
      else (param_ty_list, return_ty)
  | TVar { contents = Link ty } -> match_fun_ty num_params ty
  | TVar ({ contents = Unbound (_id, level) } as tvar) ->
      let param_ty_list =
        let rec f = function 0 -> [] | n -> new_var level :: f (n - 1) in
        f num_params
      in
      let return_ty = new_var level in
      tvar := Link (TArrow (param_ty_list, return_ty));
      (param_ty_list, return_ty)
  | ty -> error ("Expected function type, got " ^ string_of_ty ty)

let rec infer env level = function
  | Parsed_ast.Identifier name -> (
      try
        let ty = instantiate level (Env.lookup env name) in
        (Typed_ast.Identifier (name, ty), ty)
      with Not_found ->
        raise
          (TypeError
             ("Unbound variable: " ^ name ^ " at level " ^ string_of_int level))
      )
  | Parsed_ast.LetIn (var_name, value_expr, body_expr) ->
      let typed_value_expr, value_expr_ty = infer env (level + 1) value_expr in
      let value_expr_gty = generalize level value_expr_ty in
      let env' = Env.extend env var_name value_expr_gty in
      let typed_body_expr, body_expr_ty = infer env' level body_expr in
      ( Typed_ast.LetIn
          (var_name, typed_value_expr, typed_body_expr, body_expr_ty),
        body_expr_ty )
  | Parsed_ast.Lambda (params, body_expr, return_ty) ->
      let param_names, param_tys = List.split params in
      let env' =
        List.fold_left2
          (fun env param_name param_ty -> Env.extend env param_name param_ty)
          env param_names param_tys
      in
      let typed_body_expr, body_expr_ty = infer env' level body_expr in
      let body_expr_gty = generalize level body_expr_ty in
      unify body_expr_gty return_ty;
      (Typed_ast.Lambda (params, typed_body_expr, return_ty), return_ty)
  | Parsed_ast.Call (callee_expr, arg_exprs) ->
      let typed_callee_expr, callee_expr_ty = infer env level callee_expr in
      let param_tys, return_ty =
        match_fun_ty (List.length arg_exprs) callee_expr_ty
      in
      (* Validate parameter and argument types match *)
      let typed_arg_exprs =
        List.map2
          (fun param_ty arg_expr ->
            let typed_arg_expr, arg_expr_ty = infer env level arg_expr in
            unify param_ty arg_expr_ty;
            typed_arg_expr)
          param_tys arg_exprs
      in
      (Typed_ast.Call (typed_callee_expr, typed_arg_exprs, return_ty), return_ty)
  | Parsed_ast.StringLiteral s -> (Typed_ast.StringLiteral s, TConst "string")
  | Parsed_ast.Integer i -> (Typed_ast.Integer i, TConst "int")
  | Parsed_ast.Boolean b -> (Typed_ast.Boolean b, TConst "bool")
  | Parsed_ast.Unit -> (Typed_ast.Unit, TConst "unit")
  | Parsed_ast.If (cond_expr, then_expr, else_expr) ->
      let typed_cond_expr, cond_expr_ty = infer env level cond_expr in
      let typed_then_expr, then_expr_ty = infer env level then_expr in
      let typed_else_expr, else_expr_ty = infer env level else_expr in
      unify cond_expr_ty (TConst "bool");
      unify then_expr_ty else_expr_ty;
      ( Typed_ast.If
          (typed_cond_expr, typed_then_expr, typed_else_expr, then_expr_ty),
        then_expr_ty )
  | Parsed_ast.BinOp (bin_op, lhs_expr, rhs_expr) ->
      let typed_lhs_expr, lhs_expr_ty = infer env level lhs_expr in
      let typed_rhs_expr, rhs_expr_ty = infer env level rhs_expr in
      unify lhs_expr_ty rhs_expr_ty;
      let bin_op_ty =
        match bin_op with
        | BinOpPlus | BinOpMinus | BinOpMult | BinOpDiv | BinOpRem ->
            TConst "int"
        | BinOpLessThan | BinOpGreaterThan | BinOpLessThanEqual
        | BinOpGreaterThanEqual | BinOpAnd | BinOpOr | BinOpEqual
        | BinOpNotEqual ->
            TConst "bool"
      in
      ( Typed_ast.BinOp (bin_op, typed_lhs_expr, typed_rhs_expr, bin_op_ty),
        bin_op_ty )
  | Parsed_ast.UnOp (un_op, expr) ->
      let typed_expr, expr_ty = infer env level expr in
      let un_op_ty =
        match un_op with UnOpNegate -> TConst "int" | UnOpNot -> TConst "bool"
      in
      unify expr_ty un_op_ty;
      (Typed_ast.UnOp (un_op, typed_expr, un_op_ty), un_op_ty)
  | Parsed_ast.List _expr_list ->
      raise (TypeError "[List] Unsupported type")
      (* let typed_elem_expr, elem_expr_ty = infer env level (List.hd expr_list) in
        List.iter
          (fun expr ->
            let typed_expr, expr_ty = infer env level expr in
            unify expr_ty elem_expr_ty)
          (List.tl expr_list);
        Typed_ast.List (expr_list, TList elem_expr_ty), TList elem_expr_ty *)

let annotate_top_level_decl env level = function
  | Parsed_ast.Let (name, expr) ->
      let typed_expr, expr_ty = infer env (level + 1) expr in
      let expr_gty = generalize level expr_ty in
      let env' = Env.extend env name expr_gty in
      (env', Typed_ast.Let (name, typed_expr, expr_ty))
  | Parsed_ast.Function (name, params, body_expr, return_ty) ->
      let param_names, param_tys = List.split params in
      let env' =
        List.fold_left2
          (fun env param_name param_ty -> Env.extend env param_name param_ty)
          env param_names param_tys
      in
      let fn_ty = TArrow (param_tys, return_ty) in
      let env'' = Env.extend env' name fn_ty in
      let typed_body_expr, body_expr_ty = infer env'' level body_expr in
      let body_expr_gty = generalize level body_expr_ty in
      unify body_expr_gty return_ty;
      (env'', Typed_ast.Function (name, params, typed_body_expr, return_ty))

let annotate_top_level_decls env level decls =
  let updated_env, decls =
    List.fold_left
      (fun (env, decls) decl ->
        let new_env, new_decl = annotate_top_level_decl env level decl in
        (new_env, decls @ [ new_decl ]))
      (env, []) decls
  in
  (updated_env, decls)

let annotate_program (program : Parsed_ast.program) : Typed_ast.program =
  let env = Env.empty in
  let level = 0 in
  match program with
  | Parsed_ast.Program (_, decls) ->
      let _env', typed_decls = annotate_top_level_decls env level decls in
      Typed_ast.Program (typed_decls, TConst "unit")
