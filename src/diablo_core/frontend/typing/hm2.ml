open Expr

let rec string_of_ty = function
  | TConst name -> name
  | TApp(ty, ty_arg_list) ->
    "(" ^ string_of_ty ty ^ " " ^ String.concat " " (List.map string_of_ty ty_arg_list) ^ ")"
  | TArrow(param_ty_list, return_ty) ->
    "(" ^ String.concat " -> " (List.map string_of_ty param_ty_list) ^ " -> " ^ string_of_ty return_ty ^ ")"
  | TVar {contents = Link ty} -> string_of_ty ty
  | TVar {contents = Generic id} -> "gen_" ^ string_of_int id
  | TVar {contents = Unbound(id, _)} -> "unbound_" ^ string_of_int id
  | TList ty -> "list " ^ string_of_ty ty
  | TRecord fields ->
    "{" ^ String.concat ", " (List.map (fun (name, ty) -> name ^ ": " ^ string_of_ty ty) fields) ^ "}"


let current_id = ref 0

let next_id () =
  let id = !current_id in
  current_id := id + 1;
  id

let reset_id () = current_id := 0

let new_var level = TVar (ref (Unbound (next_id (), level)))
let new_gen_var () = TVar (ref (Generic(next_id())))

exception TypeError of string
let error msg = raise (TypeError msg)

module Env = struct
  module StringMap = Map.Make (String)
  type env = ty StringMap.t

  let empty = StringMap.empty
  let extend env name ty = StringMap.add name ty env
  let lookup env name = StringMap.find name env
end

let occurs_check_adjust_levels tvar_id tvar_level ty =
	let rec f = function
		| TVar {contents = Link ty} -> f ty
		| TVar {contents = Generic _} -> assert false
		| TVar ({contents = Unbound(other_id, other_level)} as other_tvar) ->
				if other_id = tvar_id then
					error "recursive types"
				else
					if other_level > tvar_level then
						other_tvar := Unbound(other_id, tvar_level)
					else
						()
		| TApp(ty, ty_arg_list) ->
				f ty ;
				List.iter f ty_arg_list
		| TArrow(param_ty_list, return_ty) ->
				List.iter f param_ty_list ;
				f return_ty
		| TConst _ -> ()
    | TList ty -> f ty
    | TRecord fields ->
      List.iter (fun (_, ty) -> f ty) fields
	in
	f ty

let rec unify ty1 ty2 =
  if ty1 == ty2 then () else
  match (ty1, ty2) with
    | TConst name1, TConst name2 when name1 = name2 -> ()
    | TApp(ty1, ty_arg_list1), TApp(ty2, ty_arg_list2) ->
      unify ty1 ty2 ;
      List.iter2 unify ty_arg_list1 ty_arg_list2
    | TArrow(param_ty_list1, return_ty1), TArrow(param_ty_list2, return_ty2) ->
      List.iter2 unify param_ty_list1 param_ty_list2;
      unify return_ty1 return_ty2
    | TVar {contents = Link ty1}, ty2 | ty1, TVar {contents = Link ty2} ->
      unify ty1 ty2
    | TVar {contents = Unbound(id1, _)}, TVar {contents = Unbound(id2, _)} when id1 = id2 ->
      assert false
    | TVar ({contents = Unbound(id, level)} as tvar), ty
    | ty, TVar ({contents = Unbound(id, level)} as tvar) ->
      occurs_check_adjust_levels id level ty;
      tvar := Link ty
    | _, _ -> error "Cannot unify types"

let rec generalize level = function
  | TVar {contents = Unbound(id, other_level)} when other_level > level ->
    TVar (ref (Generic(id)))
  | TApp(ty, ty_arg_list) ->
    TApp(generalize level ty, List.map (generalize level) ty_arg_list)
  | TArrow(param_ty_list, return_ty) ->
    TArrow(List.map (generalize level) param_ty_list, generalize level return_ty)
  | TVar {contents = Link ty} -> generalize level ty
  | TVar {contents = Generic _} | TVar {contents = Unbound _} | TConst _ as ty -> ty
  | TList ty -> TList (generalize level ty)
  | TRecord fields -> TRecord (List.map (fun (name, ty) -> name, generalize level ty) fields)

let instantiate level ty =
  let id_var_map = Hashtbl.create 10 in
  let rec f ty = match ty with
    | TConst _ -> ty
    | TVar {contents = Link ty} -> f ty
    | TVar {contents = Generic id} -> begin
      try
        Hashtbl.find id_var_map id
      with Not_found ->
        let var = new_var level in
        Hashtbl.add id_var_map id var;
        var
    end
    | TVar {contents = Unbound _} -> ty
    | TApp(ty, ty_arg_list) ->
      TApp(f ty, List.map f ty_arg_list)
    | TArrow(param_ty_list, return_ty) ->
      TArrow(List.map f param_ty_list, f return_ty)
    | TList ty -> TList (f ty)
    | TRecord fields -> TRecord (List.map (fun (name, ty) -> name, f ty) fields)
  in
  f ty

let rec match_fun_ty num_params = function
  | TArrow(param_ty_list, return_ty) ->
    if List.length param_ty_list <> num_params then
      error ("Expected " ^ string_of_int num_params ^ " arguments, got " ^ string_of_int (List.length param_ty_list))
    else
      param_ty_list, return_ty
  | TVar {contents = Link ty} -> match_fun_ty num_params ty
  | TVar ({contents = Unbound(_id, level)} as tvar) ->
    let param_ty_list =
      let rec f = function
        | 0 -> []
        | n -> new_var level :: f (n - 1)
      in
      f num_params
    in
    let return_ty = new_var level in
    tvar := Link (TArrow(param_ty_list, return_ty));
    param_ty_list, return_ty
  | _ -> error "Expected function type"

let rec infer env level = function
  | Identifier name -> begin
    try
      instantiate level (Env.lookup env name)
    with Not_found -> raise (TypeError ("Unbound variable: " ^ name))
  end
  (* | Fun(param_list, body_expr) ->
    let param_ty_list = List.map (fun _ -> new_var level) param_list in
    let fn_env = List.fold_left2
      (fun env param_name param_ty -> Env.extend env param_name param_ty)
      env param_list param_ty_list
    in
    let return_ty = infer fn_env level body_expr in
    TArrow(param_ty_list, return_ty) *)
  | Fun(param_list, body_expr, return_ty) ->
    Printf.printf "Annotated type: %s\n" (string_of_ty return_ty);
    (* get explicit param types *)
    let param_name_list, param_ty_list = List.split param_list in
    let fn_env = List.fold_left2
      (fun env param_name param_ty -> Env.extend env param_name param_ty)
      env param_name_list param_ty_list
    in
    let return_ty = infer fn_env level body_expr in
    TArrow(param_ty_list, return_ty)
  | Let(var_name, value_expr, body_expr) ->
    let var_ty = infer env (level + 1) value_expr in
    let generalized_ty = generalize level var_ty in
    infer (Env.extend env var_name generalized_ty) level body_expr
  | Call(fn_expr, arg_list) ->
    let param_ty_list, return_ty =
      match_fun_ty (List.length arg_list) (infer env level fn_expr)
    in
    List.iter2
      (fun param_ty arg_expr -> unify param_ty (infer env level arg_expr))
      param_ty_list arg_list
    ;
    return_ty

  | StringLiteral _ -> TConst "str"
  | Integer _ -> TConst "int"
  | Boolean _ -> TConst "bool"
  | Unit _ -> TConst "unit"

  | If(cond_expr, then_expr, else_expr) ->
    let cond_ty = infer env level cond_expr in
    let then_ty = infer env level then_expr in
    let else_ty = infer env level else_expr in
    unify cond_ty (TConst "bool");
    unify then_ty else_ty;
    then_ty

  | BinOp(_op, lhs_expr, rhs_expr) ->
    let lhs_ty = infer env level lhs_expr in
    let rhs_ty = infer env level rhs_expr in
    let return_ty = new_var level in
    unify lhs_ty return_ty;
    unify rhs_ty return_ty;
    return_ty

  | UnOp(_op, expr) ->
    let ty = infer env level expr in
    let return_ty = new_var level in
    unify ty return_ty;
    return_ty

  | List(expr_list) ->
    let elem_ty = infer env level (List.hd expr_list) in
    List.iter
      (fun expr -> unify elem_ty (infer env level expr))
      (List.tl expr_list);
    TList(elem_ty)

  | Record(fields) ->
    let field_tys = List.map
      (fun (field_name, expr) ->
        let field_ty = infer env level expr in
        (field_name, field_ty)
      ) fields in
    TRecord field_tys