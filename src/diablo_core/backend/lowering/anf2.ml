open Desugaring
open Ast.Ast_types

let gensym =
  let counter = ref 0 in
  fun prefix ->
    let name = prefix ^ string_of_int !counter in
    incr counter;
    name

let string_of_anf_result = function
  | b, e -> "Bindings: " ^ String.concat ", " (List.map (fun (n, e) -> n ^ " = " ^ Pprint_dast.string_of_expr e) b) ^ "\n" ^ "Expression: " ^ Pprint_dast.string_of_expr e

type anf_result = (name * Desugared_ast.expr) list * Desugared_ast.expr

let is_atomic = function
  | Desugared_ast.Identifier _ | Desugared_ast.Integer _ | Desugared_ast.Boolean _ | Desugared_ast.StringLiteral _ | Desugared_ast.Unit -> true
  | _ -> false

let rec anf_expr (e : Desugared_ast.expr) : anf_result =
  match e with
  | Integer _ | Boolean _ | StringLiteral _ | Unit | Identifier _ -> ([], e)

  | BinOp (op, e1, e2, ty) ->
      let b1, a1 = anf_expr e1 in
      let b2, a2 = anf_expr e2 in
      let x = gensym "bin" in
      let binding = (x, Desugared_ast.BinOp (op, a1, a2, ty)) in
      (b1 @ b2 @ [binding], Identifier (x, ty))

  | UnOp (op, e, ty) ->
      let b, a = anf_expr e in
      let x = gensym "un" in
      let binding = (x, Desugared_ast.UnOp (op, a, ty)) in
      (b @ [binding], Identifier (x, ty))

  | Call (f, args, ty) ->
      let b_f, a_f = anf_expr f in
      let args_anf = List.map anf_expr args in
      let bs, as_ = List.split args_anf in
      let all_bindings = b_f @ List.flatten bs in
      let x = gensym "call" in
      let binding = (x, Desugared_ast.Call (a_f, as_, ty)) in
      (all_bindings @ [binding], Identifier (x, ty))

  | Lambda (params, body, ty) ->
      let b_body, a_body = anf_expr body in
      let lam = Desugared_ast.Lambda (params, List.fold_right (fun (n, e) acc -> Desugared_ast.LetIn (n, e, acc, ty)) b_body a_body, ty) in
      let x = gensym "lam" in
      ([x, lam], Identifier (x, ty))

  | LetIn (n, e1, e2, _ty) ->
      let b1, a1 = anf_expr e1 in
      let b2, a2 = anf_expr e2 in
      let full = b1 @ [(n, a1)] @ b2 in
      (full, a2)

  | If (cond, then_e, else_e, ty) ->
      let b_cond, a_cond = anf_expr cond in
      let then_b, then_a = anf_expr then_e in
      let else_b, else_a = anf_expr else_e in

      (* Create a join point *)
      let j = gensym "join" in
      let j_arg = gensym "x" in
      let k_body = (j_arg, ty) in
      let cont_body = Desugared_ast.Identifier (j_arg, ty) in
      let kont = Desugared_ast.Lambda ([k_body],
                          List.fold_right (fun (n, e) acc -> Desugared_ast.LetIn (n, e, acc, ty))
                            [] cont_body,
                          ty) in

      let then_call = List.fold_right (fun (n, e) acc -> Desugared_ast.LetIn (n, e, acc, ty)) then_b (Call (Identifier (j, ty), [then_a], ty)) in
      let else_call = List.fold_right (fun (n, e) acc -> Desugared_ast.LetIn (n, e, acc, ty)) else_b (Call (Identifier (j, ty), [else_a], ty)) in
      let if_expr = Desugared_ast.If (a_cond, then_call, else_call, ty) in
      let result = Desugared_ast.LetIn (j, kont, if_expr, ty) in
      (b_cond, result)

  | List (es, ty) ->
      let bs_as = List.map anf_expr es in
      let bs, as_ = List.split bs_as in
      let x = gensym "list" in
      let binding = (x, Desugared_ast.List (as_, ty)) in
      (List.flatten bs @ [binding], Identifier (x, ty))

let anf_decl = function
  | Desugared_ast.Let (n, e, ty) ->
      let bindings, e' = anf_expr e in
      let decls = List.map (fun (n, e) -> Desugared_ast.Let (n, e, ty)) bindings in
      decls @ [Desugared_ast.Let (n, e', ty)]

let anf_program (Desugared_ast.Program (decls, ty)) =
  let decls' = List.flatten (List.map anf_decl decls) in
  Desugared_ast.Program (decls', ty)