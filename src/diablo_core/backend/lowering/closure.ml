open Anf
open Fresh
module VS = Set.Make (String)

let rec fvs_cexpr (e : cexpr) : VS.t =
  match e with
  | Halt a -> fvs_aexpr a
  | Fun (fname, args, body, cont) ->
      let arg_set = VS.of_list (fname :: args) in
      let body_fvs = VS.diff (fvs_cexpr body) arg_set in
      VS.union body_fvs (fvs_cexpr cont)
  | Join (_name, param_opt, body, cont) ->
      let body_fvs =
        match param_opt with
        | Some p -> VS.remove p (fvs_cexpr body)
        | None -> fvs_cexpr body
      in
      VS.union body_fvs (fvs_cexpr cont)
  | Jump (_, aopt) -> fvs_aexpr_opt aopt
  | App (_, func_name, args, body) ->
      let arg_fvs =
        List.fold_left (fun acc a -> VS.union acc (fvs_aexpr a)) VS.empty args
      in
      VS.union (VS.singleton func_name) (VS.union arg_fvs (fvs_cexpr body))
  | BinOp (_, _, a1, a2, body) ->
      VS.union (fvs_aexpr a1) (VS.union (fvs_aexpr a2) (fvs_cexpr body))
  | UnOp (_, _, a, body) -> VS.union (fvs_aexpr a) (fvs_cexpr body)
  | If (cond, then_e, else_e) ->
      VS.union (fvs_aexpr cond) (VS.union (fvs_cexpr then_e) (fvs_cexpr else_e))
  | Tuple (name, args, body) ->
      let arg_fvs =
        List.fold_left (fun acc a -> VS.union acc (fvs_aexpr a)) VS.empty args
      in
      VS.union arg_fvs (VS.remove name (fvs_cexpr body))
  | Let (name, value, body) ->
      VS.union (fvs_aexpr value) (VS.remove name (fvs_cexpr body))
  | Proj (name, tuple_var, _, body) ->
      VS.add tuple_var (VS.remove name (fvs_cexpr body))

and fvs_aexpr (a : aexpr) : VS.t =
  match a with
  | AIdentifier x -> VS.singleton x
  | AGlobal _ -> VS.empty
  | AInteger _ -> VS.empty
  | ABoolean _ -> VS.empty
  | AStringLiteral _ -> VS.empty
  | AUnit -> VS.empty

and fvs_aexpr_opt (aopt : aexpr option) : VS.t =
  match aopt with Some a -> fvs_aexpr a | None -> VS.empty

let convert =
  let rec go : cexpr -> cexpr = function
    | Halt atom -> Halt atom
    | Join (name, param, body, cont) -> Join (name, param, go body, go cont)
    | Jump (name, atom) -> Jump (name, atom)
    | BinOp (name, op, lhs, rhs, body) -> BinOp (name, op, lhs, rhs, go body)
    | UnOp (name, op, expr, body) -> UnOp (name, op, expr, go body)
    | If (cond, then_expr, else_expr) -> If (cond, go then_expr, go else_expr)
    | Let (name, value, body) -> Let (name, value, go body)
    | Tuple (name, args, body) -> Tuple (name, args, go body)
    | Proj (name, x, i, body) -> Proj (name, x, i, go body)
    | Fun (name, args, body, e') ->
        let env = NameGen.fresh "env" in
        let fvs = VS.(elements (diff (fvs_cexpr body) (of_list args))) in
        let proj (e, i) x = (Proj (x, env, i, e), i + 1) in
        let body = fst (List.fold_left proj (go body, 1) fvs) in
        let e' =
          let vs = List.map (fun v -> AIdentifier v) fvs in
          Tuple (name, AGlobal name :: vs, go e')
        in
        Fun (name, env :: args, body, e')
    | App (result_name, callee, args, body) ->
        let ptr = NameGen.fresh callee in
        Proj
          ( ptr,
            callee,
            0,
            App (result_name, ptr, AIdentifier callee :: args, go body) )
  in
  go

let convert_program (l : (string * cexpr) list) : (string * cexpr) list =
  List.map (fun (x, e) -> (x, convert e)) l
