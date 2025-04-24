open Ast.Ast_types
open Desugaring.Desugared_ast
open Fresh

type aexpr =
  | AIdentifier of name
  | AInteger of int
  | ABoolean of bool
  | AStringLiteral of string
  | AUnit
  | AGlobal of name

type cexpr =
  | Halt of aexpr
  | Fun of name * name list * cexpr * cexpr
  | Join of name * name option * cexpr * cexpr
  | Jump of name * aexpr option
  | App of name * name * aexpr list * cexpr
  | BinOp of name * bin_op * aexpr * aexpr * cexpr
  | UnOp of name * un_op * aexpr * cexpr
  | If of aexpr * cexpr * cexpr
  | Tuple of name * aexpr list * cexpr
  | Let of name * aexpr * cexpr
  | Proj of name * name * int * cexpr

let rec pp_aexpr = function
  | AIdentifier x -> x
  | AInteger n -> string_of_int n
  | ABoolean b -> string_of_bool b
  | AStringLiteral s -> "\"" ^ s ^ "\""
  | AUnit -> "()"
  | AGlobal x -> x

and pp_cexpr = function
  | Halt a -> "Halt " ^ pp_aexpr a
  | Fun (name, args, body, e') ->
      let arg_str = String.concat ", " args in
      "Fun (" ^ name ^ ", [" ^ arg_str ^ "], " ^ pp_cexpr body ^ ")" ^ ", "
      ^ pp_cexpr e'
  | Join (name, params, body, continue) ->
      let params_str = String.concat ", " (Option.to_list params) in
      "Join (" ^ name ^ ", [" ^ params_str ^ "], " ^ pp_cexpr body ^ ", "
      ^ pp_cexpr continue ^ ")"
  | Jump (name, arg) ->
      let arg_str = Option.map pp_aexpr arg in
      "Jump (" ^ name ^ ", " ^ Option.value ~default:"None" arg_str ^ ")"
  | App (name, fn_name, args, body) ->
      let arg_str = String.concat ", " (List.map pp_aexpr args) in
      "App (" ^ name ^ ", " ^ fn_name ^ ", [" ^ arg_str ^ "], " ^ pp_cexpr body
      ^ ")"
  | BinOp (name, _op, lhs, rhs, body) ->
      "BinOp (" ^ name ^ ", " ^ pp_aexpr lhs ^ ", " ^ pp_aexpr rhs ^ ", "
      ^ pp_cexpr body ^ ")"
  | UnOp (name, _op, expr, body) ->
      "UnOp (" ^ name ^ ", " ^ pp_aexpr expr ^ ", " ^ pp_cexpr body ^ ")"
  | If (cond, then_expr, else_expr) ->
      "If (" ^ pp_aexpr cond ^ ", " ^ pp_cexpr then_expr ^ ", "
      ^ pp_cexpr else_expr ^ ")"
  | Let (name, value, body) ->
      "Let (" ^ name ^ ", " ^ pp_aexpr value ^ ", " ^ pp_cexpr body ^ ")"
  | Proj (name, x, i, body) ->
      "Proj (" ^ name ^ ", " ^ x ^ ", " ^ string_of_int i ^ ", " ^ pp_cexpr body
      ^ ")"
  | Tuple (name, args, body) ->
      let arg_str = String.concat ", " (List.map pp_aexpr args) in
      "Tuple (" ^ name ^ ", [" ^ arg_str ^ "], " ^ pp_cexpr body ^ ")"

let convert =
  let ( let* ) = ( @@ ) in
  let rec go (e : expr) (k : aexpr -> cexpr) : cexpr =
    match e with
    | Identifier (x, _) -> k (AIdentifier x)
    | Integer n -> k (AInteger n)
    | Boolean b -> k (ABoolean b)
    | StringLiteral s -> k (AStringLiteral s)
    | Unit -> k AUnit
    | Lambda (args, body, _) ->
        let fn_name = NameGen.fresh "f" in
        let arg_names = List.map fst args in
        let anf_body = go body (fun atom -> Halt atom) in
        Fun (fn_name, arg_names, anf_body, k (AIdentifier fn_name))
    | BinOp (op, lhs, rhs, _) ->
        let* lhs = go lhs in
        let* rhs = go rhs in
        let result_name = NameGen.fresh "r" in
        BinOp (result_name, op, lhs, rhs, k (AIdentifier result_name))
    | UnOp (op, expr, _) ->
        let* expr = go expr in
        let result_name = NameGen.fresh "r" in
        UnOp (result_name, op, expr, k (AIdentifier result_name))
    | If (cond_expr, then_expr, else_expr, _) ->
        let* cond_expr = go cond_expr in
        let j, p = (NameGen.fresh "j", NameGen.fresh "p") in
        let join v = Jump (j, Some v) in
        Join
          ( j,
            Some p,
            k (AIdentifier p),
            If (cond_expr, go then_expr join, go else_expr join) )
    | LetIn (name, expr, body, _) ->
        let* expr = go expr in
        Let (name, expr, go body k)
    | Call (callee, args, _) -> (
        let* callee = go callee in
        let* args = anf_of_args args [] in
        match callee with
        | AIdentifier callee ->
            let result_name = NameGen.fresh "r" in
            App (result_name, callee, args, k (AIdentifier result_name))
        | _ -> failwith "Must apply named value")
    | _ -> failwith "Not implemented"
  and anf_of_args args acc k =
    match args with
    | [] -> k (List.rev acc)
    | e :: rest -> go e (fun v -> anf_of_args rest (v :: acc) k)
  in
  Fun.flip go (fun atom -> Halt atom)

let convert_top_level_declaration (Desugaring.Desugared_ast.Let (x, e, _)) =
  (x, convert e)

let pp_top_level_declaration (x, e) = x ^ " = " ^ pp_cexpr e

let pp_program (l : (string * cexpr) list) =
  String.concat "\n" (List.map pp_top_level_declaration l)

let convert_program (Program (stmts, _)) =
  let stmts = List.map convert_top_level_declaration stmts in
  stmts

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
  | ANFJoin of
      name
      * name list
      * anf_expr
      * anf_expr (* join label, params, body, continue *)
  | ANFList of name list

and anf_atom =
  | AtomVar of name
  | AtomInt of int
  | AtomBool of bool
  | AtomString of string
  | AtomUnit
  | AtomLambda of name list * anf_expr

type anf_program = ANFProgram of (name * anf_expr) list

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
              let _call_expr = ANFCall ("binop", [ lv; rv ]) in
              ANFLet (tmp, AtomVar tmp, k tmp)))
      (* Placeholder *)
  | UnOp (_, e, _) ->
      anf_of_expr e (fun v ->
          let tmp = NameGen.fresh "un" in
          ANFLet (tmp, AtomVar v, k tmp))
      (* Placeholder *)
  | Call (f, args, _) ->
      anf_of_expr f (fun fv ->
          anf_of_args args [] (fun arg_names ->
              let tmp = NameGen.fresh "call" in
              ANFLet
                (tmp, AtomVar fv, ANFCall (fv, arg_names) |> fun _c -> k tmp)))
  | LetIn (x, e1, e2, _) ->
      anf_of_expr e1 (fun v -> ANFLet (x, AtomVar v, anf_of_expr e2 k))
  | If (cond, then_e, else_e, _) ->
      anf_of_expr cond (fun v_cond ->
          let join_label = NameGen.fresh "join" in
          let join_arg = NameGen.fresh "v" in
          let then_branch =
            anf_of_expr then_e (fun v -> ANFCall (join_label, [ v ]))
          in
          let else_branch =
            anf_of_expr else_e (fun v -> ANFCall (join_label, [ v ]))
          in
          ANFJoin
            ( join_label,
              [ join_arg ],
              k join_arg,
              ANFIf (v_cond, then_branch, else_branch) ))
  | List (items, _) ->
      anf_of_args items [] (fun _names ->
          let tmp = NameGen.fresh "list" in
          ANFLet (tmp, AtomVar tmp, k tmp))

and anf_of_args args acc k =
  match args with
  | [] -> k (List.rev acc)
  | e :: rest -> anf_of_expr e (fun v -> anf_of_args rest (v :: acc) k)

let anf_of_top (decl : top_level_declaration) : name * anf_expr =
  match decl with Let (x, e, _) -> (x, anf_of_expr e (fun v -> ANFVar v))

let anf_of_program (Program (decls, _)) : anf_program =
  ANFProgram (List.map anf_of_top decls)

let rec string_of_anf_program (ANFProgram decls) =
  String.concat "\n"
    (List.map (fun (x, e) -> x ^ " = " ^ string_of_anf_expr e) decls)

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
      "join " ^ label ^ "(" ^ params_str ^ ") = " ^ body_str ^ " in\n"
      ^ continue_str
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
