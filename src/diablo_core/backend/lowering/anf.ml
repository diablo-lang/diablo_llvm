open Ast.Ast_types
open Desugaring.Desugared_ast
open Fresh

(* ANF types *)

type anf_expr =
  | ANFVar of name
  | ANFInt of int
  | ANFBool of bool
  | ANFString of string
  | ANFUnit
  | ANFLambda of name list * anf_expr
  | ANFCall of name * name list
  | ANFIf of name * anf_expr * anf_expr
  | ANFLet of name * anf_atom * anf_expr
  | ANFJoin of name * name list * anf_expr * anf_expr  (* join label, params, body, continue *)
  | ANFList of name list

and anf_atom =
  | AtomVar of name
  | AtomInt of int
  | AtomBool of bool
  | AtomString of string
  | AtomUnit
  | AtomLambda of name list * anf_expr

let rec anf_of_expr (e : expr) (k : name -> anf_expr) : anf_expr =
  match e with
  | Identifier (x, _) -> k x
  | Integer n -> 
      let tmp = NameGen.fresh "int" in
      ANFLet (tmp, AtomInt n, k tmp)
  | Boolean b -> 
      let tmp = NameGen.fresh "bool" in
      ANFLet (tmp, AtomBool b, k tmp)
  | StringLiteral s -> 
      let tmp = NameGen.fresh "str" in
      ANFLet (tmp, AtomString s, k tmp)
  | Unit -> 
      let tmp = NameGen.fresh "unit" in
      ANFLet (tmp, AtomUnit, k tmp)
  | Lambda (args, body, _) ->
      let arg_names = List.map fst args in
      let anf_body = anf_of_expr body (fun x -> ANFVar x) in
      let tmp = NameGen.fresh "lam" in
      ANFLet (tmp, AtomLambda (arg_names, anf_body), k tmp)
  | BinOp (_, lhs, rhs, _) ->
      anf_of_expr lhs (fun lv ->
        anf_of_expr rhs (fun rv ->
          let tmp = NameGen.fresh "bin" in
          (* Assuming a helper op_bin : name -> name -> name *)
          let _call_expr = ANFCall ("binop", [lv; rv]) in
          ANFLet (tmp, AtomVar tmp, k tmp))) (* Placeholder *)
  | UnOp (_, e, _) ->
      anf_of_expr e (fun v ->
        let tmp = NameGen.fresh "un" in
        ANFLet (tmp, AtomVar v, k tmp)) (* Placeholder *)
  | Call (f, args, _) ->
      anf_of_expr f (fun fv ->
        anf_of_args args [] (fun arg_names ->
          let tmp = NameGen.fresh "call" in
          ANFLet (tmp, AtomVar fv, ANFCall (fv, arg_names) |> fun _c -> k tmp)))
  | LetIn (x, e1, e2, _) ->
      anf_of_expr e1 (fun v ->
        ANFLet (x, AtomVar v, anf_of_expr e2 k))
  | If (cond, then_e, else_e, _) ->
      anf_of_expr cond (fun v_cond ->
        let join_label = NameGen.fresh "join" in
        let join_arg = NameGen.fresh "v" in
        let then_branch = anf_of_expr then_e (fun v -> ANFCall (join_label, [v])) in
        let else_branch = anf_of_expr else_e (fun v -> ANFCall (join_label, [v])) in
        ANFJoin (join_label, [join_arg], k join_arg, ANFIf (v_cond, then_branch, else_branch)))
  | List (items, _) ->
      anf_of_args items [] (fun _names ->
        let tmp = NameGen.fresh "list" in
        ANFLet (tmp, AtomVar tmp, k tmp)) (* Placeholder for list construction *)

and anf_of_args args acc k =
  match args with
  | [] -> k (List.rev acc)
  | e :: rest ->
      anf_of_expr e (fun v ->
        anf_of_args rest (v :: acc) k)


let anf_of_top (decl : top_level_declaration) : (name * anf_expr) =
  match decl with
  | Let (x, e, _) ->
      (x, anf_of_expr e (fun v -> ANFVar v))

let anf_of_program (Program (decls, _)) : (name * anf_expr) list =
  List.map anf_of_top decls

let rec string_of_anf_program (Program (decls, _)) =
  let decls = List.map anf_of_top decls in
  String.concat "\n" (List.map (fun (x, e) -> x ^ " = " ^ string_of_anf_expr e) decls)

and string_of_anf_expr (e : anf_expr) =
  match e with
  | ANFVar x -> x
  | ANFInt n -> string_of_int n
  | ANFBool b -> string_of_bool b
  | ANFString s -> "\"" ^ s ^ "\""
  | ANFUnit -> "()"
  | ANFLambda (args, body) ->
      let arg_str = String.concat ", " args in
      let body_str = string_of_anf_expr body in
      "(" ^ arg_str ^ " -> " ^ body_str ^ ")"
  | ANFCall (f, args) ->
      let arg_str = String.concat ", " args in
      "(" ^ f ^ "(" ^ arg_str ^ "))"
  | ANFIf (cond, then_expr, else_expr) ->
      let then_str = string_of_anf_expr then_expr in
      let else_str = string_of_anf_expr else_expr in
      "if " ^ cond ^ " then " ^ then_str ^ " else " ^ else_str
  | ANFLet (x, value, body) ->
      let value_str = string_of_anf_atom value in
      let body_str = string_of_anf_expr body in
      "let " ^ x ^ " = " ^ value_str ^ " in\n" ^ body_str
  | ANFJoin (label, params, body, continue) ->
      let params_str = String.concat ", " params in
      let body_str = string_of_anf_expr body in
      let continue_str = string_of_anf_expr continue in
      "join " ^ label ^ "(" ^ params_str ^ ") = " ^ body_str ^ " in\n" ^ continue_str
  | ANFList items ->
      let items_str = String.concat ", " items in
      "[" ^ items_str ^ "]"

and string_of_anf_atom (a : anf_atom) =
  match a with
  | AtomVar x -> x
  | AtomInt n -> string_of_int n
  | AtomBool b -> string_of_bool b
  | AtomString s -> "\"" ^ s ^ "\""
  | AtomUnit -> "()"
  | AtomLambda (args, body) ->
      let arg_str = String.concat ", " args in
      let body_str = string_of_anf_expr body in
      "(" ^ arg_str ^ " -> " ^ body_str ^ ")"