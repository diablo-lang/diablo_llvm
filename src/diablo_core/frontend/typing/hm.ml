open Ast.Ast_types
open Parsing

exception TypeError of string

module TypeEnv = struct
  type t = (string * Type.t) list

  let empty : t = []
  let extend env (x, t) = (x, t) :: env

  let extend_many env bindings =
    List.fold_left (fun env (x, t) -> extend env (x, t)) env bindings

  let find env x =
    try List.assoc x env
    with Not_found -> raise (TypeError ("Unbound variable: " ^ x))
end

module Subst = struct
  type t = (string * Type.t) list

  let empty : t = []

  let rec apply (s : t) (t : Type.t) : Type.t =
    match t with
    | Type.TVar x -> ( try List.assoc x s with Not_found -> t)
    | Type.TFun (args, ret) -> Type.TFun (List.map (apply s) args, apply s ret)
    | _ -> t

  let compose (s1 : t) (s2 : t) : t =
    let s2' = List.map (fun (x, t) -> (x, apply s1 t)) s2 in
    s1 @ s2'
end

module Unify = struct
  exception UnificationError of string

  let rec unify (t1 : Type.t) (t2 : Type.t) : Subst.t =
    match (t1, t2) with
    | Type.TInt, Type.TInt | Type.TBool, Type.TBool | Type.TVoid, Type.TVoid ->
        Subst.empty
    | Type.TVar x, t | t, Type.TVar x ->
        if t = Type.TVar x then Subst.empty else [ (x, t) ]
    | Type.TFun (args1, ret1), Type.TFun (args2, ret2)
      when List.length args1 = List.length args2 ->
        let s1 =
          List.fold_left2
            (fun subst a1 a2 ->
              Subst.compose subst
                (unify (Subst.apply subst a1) (Subst.apply subst a2)))
            Subst.empty args1 args2
        in
        let s2 = unify (Subst.apply s1 ret1) (Subst.apply s1 ret2) in
        Subst.compose s1 s2
    | _, _ ->
        raise
          (UnificationError
             ("Cannot unify " ^ Type.to_string t1 ^ " with " ^ Type.to_string t2))
end

module Infer = struct
  let count = ref 0

  let fresh_tvar () =
    let v = "'t" ^ string_of_int !count in
    incr count;
    Type.TVar v

  let rec infer_block env block =
    match block with
    | Parsed_ast.Block exprs ->
        List.fold_left
          (fun (s, _) expr ->
            let s', t' =
              infer (List.map (fun (x, t) -> (x, Subst.apply s t)) env) expr
            in
            (Subst.compose s s', t'))
          (Subst.empty, Type.TVoid) exprs

  and infer env expr =
    match expr with
    | Parsed_ast.Identifier x -> ([], TypeEnv.find env x)
    | Parsed_ast.Integer _ -> ([], Type.TInt)
    | Parsed_ast.Boolean _ -> ([], Type.TBool)
    | Parsed_ast.StringLiteral _ -> ([], Type.TString)
    | Parsed_ast.UnOp (UnOpNegate, e) ->
        let s1, t1 = infer env e in
        let s2 = Unify.unify (Subst.apply s1 t1) Type.TInt in
        (Subst.compose s1 s2, Type.TInt)
    | Parsed_ast.UnOp (UnOpNot, e) ->
        let s1, t1 = infer env e in
        let s2 = Unify.unify (Subst.apply s1 t1) Type.TBool in
        (Subst.compose s1 s2, Type.TBool)
    | Parsed_ast.BinOp (op, e1, e2) ->
        let s1, t1 = infer env e1 in
        let s2, t2 =
          infer (List.map (fun (x, t) -> (x, Subst.apply s1 t)) env) e2
        in
        let s3 = Unify.unify (Subst.apply s2 t1) (Subst.apply s2 t2) in
        let result_type =
          match op with
          | BinOpPlus | BinOpMinus | BinOpMult | BinOpDiv | BinOpRem ->
              Type.TInt
          | BinOpAnd | BinOpOr -> Type.TBool
          | BinOpLessThan | BinOpGreaterThan | BinOpLessThanEqual
          | BinOpGreaterThanEqual ->
              Type.TBool
          | BinOpEqual | BinOpNotEqual -> Type.TBool
        in
        (Subst.compose (Subst.compose s1 s2) s3, result_type)
    | Parsed_ast.Let (x, e) ->
        let s1, t1 = infer env e in
        let _env' = TypeEnv.extend env (x, t1) in
        (s1, t1)
    | Parsed_ast.If (cond, then_branch, else_branch) ->
        let s1, t1 = infer env cond in
        let s2 = Unify.unify (Subst.apply s1 t1) Type.TBool in
        (* let s3, t2 = infer (List.map (fun (x, t) -> (x, Subst.apply s2 t)) env) then_branch in *)
        let s3, t2 =
          infer_block
            (List.map (fun (x, t) -> (x, Subst.apply s2 t)) env)
            then_branch
        in
        let s4, t3 =
          infer_block
            (List.map (fun (x, t) -> (x, Subst.apply s2 t)) env)
            else_branch
        in
        let s5 = Unify.unify (Subst.apply s4 t2) (Subst.apply s4 t3) in
        ( Subst.compose
            (Subst.compose (Subst.compose s1 s2) s3)
            (Subst.compose s4 s5),
          Subst.apply s5 t2 )
    (* TODO: Add support for ExternCall (maybe)? *)
    | Parsed_ast.Call (fn, args) | Parsed_ast.ExternCall (fn, args) -> (
        let fn_type =
          try TypeEnv.find env fn
          with Not_found -> raise (TypeError ("Undefined function: " ^ fn))
        in
        match fn_type with
        | Type.TFun (param_types, return_type) ->
            if List.length param_types <> List.length args then
              raise
                (TypeError
                   ("Function " ^ fn ^ " expected "
                   ^ string_of_int (List.length param_types)
                   ^ " arguments, but got "
                   ^ string_of_int (List.length args)))
            else
              let s1, inferred_arg_types =
                List.split (List.map (infer env) args)
              in
              let s1 = List.fold_left Subst.compose Subst.empty s1 in
              let s2 =
                List.fold_left2
                  (fun subst param_t inferred_t ->
                    Subst.compose subst
                      (Unify.unify
                         (Subst.apply subst param_t)
                         (Subst.apply subst inferred_t)))
                  s1 param_types inferred_arg_types
              in
              (s2, Subst.apply s2 return_type)
        | _ ->
            raise
              (TypeError ("Expected function, got " ^ Type.to_string fn_type)))
end

let type_check (Parsed_ast.Program (_import_stmts, functions, main_expr)) =
  (* Step 1: Collect function signatures first *)
  let env =
    List.fold_left
      (fun env (Parsed_ast.TFunction (name, params, return_type, _body)) ->
        let param_types = List.map (fun (TParam (t, _)) -> t) params in
        TypeEnv.extend env (name, Type.TFun (param_types, return_type)))
      TypeEnv.empty functions
  in

  (* Step 2: Now type-check each function body *)
  List.iter
    (fun (Parsed_ast.TFunction (_name, params, return_type, body)) ->
      let env_with_params =
        List.fold_left
          (fun env (TParam (t, param_name)) ->
            TypeEnv.extend env (param_name, t))
          env params
      in
      let _, inferred_return_type = Infer.infer_block env_with_params body in
      let _ = Unify.unify inferred_return_type return_type in
      ())
    functions;

  (* Step 3: Type check the main expression now that all functions exist *)
  let _, main_type = Infer.infer_block env main_expr in
  main_type

let rec annotate_expr env expr =
  let _s, inferred_type = Infer.infer env expr in
  match expr with
  | Parsed_ast.Identifier x -> Typed_ast.Identifier x
  | Parsed_ast.Integer n -> Typed_ast.Integer n
  | Parsed_ast.Boolean b -> Typed_ast.Boolean b
  | Parsed_ast.StringLiteral s -> Typed_ast.StringLiteral s
  | Parsed_ast.UnOp (op, e) ->
      Typed_ast.UnOp (inferred_type, op, annotate_expr env e)
  | Parsed_ast.BinOp (op, e1, e2) ->
      Typed_ast.BinOp
        (inferred_type, op, annotate_expr env e1, annotate_expr env e2)
  | Parsed_ast.Let (x, e) ->
      Typed_ast.Let (inferred_type, x, annotate_expr env e)
  | Parsed_ast.If (cond, then_branch, else_branch) ->
      Typed_ast.If
        ( inferred_type,
          annotate_expr env cond,
          annotate_block env then_branch,
          annotate_block env else_branch )
  | Parsed_ast.Call (fn, args) ->
      Typed_ast.Call (inferred_type, fn, List.map (annotate_expr env) args)
  (* TODO: Add support for ExternCall (maybe)? *)
  | Parsed_ast.ExternCall (fn, args) ->
      Typed_ast.Call (inferred_type, fn, List.map (annotate_expr env) args)

and annotate_block env (Parsed_ast.Block exprs) =
  let _, block_type = Infer.infer_block env (Parsed_ast.Block exprs) in
  Typed_ast.Block (block_type, List.map (annotate_expr env) exprs)

let annotate_function env
    (Parsed_ast.TFunction (name, params, return_type, body)) =
  let param_types = List.map (fun (Ast.Ast_types.TParam (t, _)) -> t) params in
  let env =
    TypeEnv.extend_many env
      (List.combine
         (List.map (fun (Ast.Ast_types.TParam (_, p)) -> p) params)
         param_types)
  in
  let typed_body = annotate_block env body in
  Typed_ast.TFunction (name, params, return_type, typed_body)

let convert_to_typed_ast
    (Parsed_ast.Program (_import_stmts, functions, main_block)) =
  (* First, register function signatures in the environment *)
  let env =
    List.fold_left
      (fun env (Parsed_ast.TFunction (name, params, return_type, _)) ->
        let param_types = List.map (fun (TParam (t, _)) -> t) params in
        TypeEnv.extend env (name, Type.TFun (param_types, return_type)))
      TypeEnv.empty functions
  in

  (* Then, annotate each function *)
  let typed_functions = List.map (fun f -> annotate_function env f) functions in

  (* Finally, annotate the main block *)
  let typed_main_block = annotate_block env main_block in

  Typed_ast.Program (typed_functions, typed_main_block)
