(* open Anf
open Alpha

module Closure = struct
  let convert =
    let rec go : Anf_ast.complex_expr -> Anf_ast.complex_expr = function
      | Anf_ast.Halt name -> Anf_ast.Halt name
      | Anf_ast.Join (name, name_opt, body, env) -> Anf_ast.Join (name, name_opt, go body, go env)
      | Anf_ast.Jump (name, expr_opt) -> Anf_ast.Jump (name, expr_opt)
      | Anf_ast.BinOp (name, op, lhs, rhs, env) -> Anf_ast.BinOp (name, op, lhs, rhs, go env)
      | Anf_ast.UnOp (name, op, expr, env) -> Anf_ast.UnOp (name, op, expr, go env)
      | Anf_ast.LetIn (name, expr, body) -> Anf_ast.LetIn (name, expr, go body)
      | Anf_ast.If (cond_expr, then_expr, else_expr) -> Anf_ast.If (cond_expr, go then_expr, go else_expr)
      | Anf_ast.Tuple (name, args, env) -> Anf_ast.Tuple (name, args, go env)
      | Anf_ast.List (name, args, env) -> Anf_ast.List (name, args, go env)
      | Anf_ast.Proj (name, expr, idx, env) -> Anf_ast.Proj (name, expr, idx, go env)
      | Anf_ast.Fun (f, xs, e, e') ->
        let env = fresh "env" in
        let fvs = VS.(elements (diff (fvs e) (of_list xs))) in
                (* let x = env[i] in ... e *)
        let proj (e, i) x =
          (Anf_ast.Proj (x, env, i, e), i+1)
        in
        let e =
          fst (List.fold_left proj (go e, 1) fvs)
        in
        (* let f = (@f, fvs...) in e' *)
        let e' =
          let vs = List.map (fun v -> Anf_ast.Identifier v) fvs in
          Anf_ast.Tuple (f, Glob f :: vs, go e')
        in
        Fun (f, env :: xs, e, e')
      | Anf_ast.App (r, f, vs, e) ->
        let ptr = fresh f in
        Proj(ptr, f, 0, App(r, ptr, Anf_ast.Identifier f :: vs, go e))
    in go
end *)