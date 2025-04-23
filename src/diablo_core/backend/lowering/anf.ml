open Desugaring
open Ast.Ast_types
open Fresh

module Anf_ast = struct
  type atom_expr =
    | Identifier of name
    | Integer of int
    | Boolean of bool
    | StringLiteral of string
    | Unit
  
  type complex_expr =
    | Halt of atom_expr
    | Fun of name * (name * ty) list * complex_expr * complex_expr
    | Join of name * name option * complex_expr * complex_expr
    | Jump of name * atom_expr option
    | App of name * name * atom_expr list * complex_expr
    | BinOp of name * bin_op * atom_expr * atom_expr * complex_expr
    | UnOp of name * un_op * atom_expr * complex_expr
    | If of atom_expr * complex_expr * complex_expr
    | Tuple of name * atom_expr list * complex_expr
    | List of name * atom_expr list * complex_expr
    | Proj of name * name * int * complex_expr
    | LetIn of name * atom_expr * complex_expr

  type program = Program of complex_expr list
end

module Anf = struct
  let convert =
    let ( let* ) = (@@) in
    let mk_halt name = Anf_ast.Halt name in
    let rec convert_expr (expr : Desugared_ast.expr) (k : Anf_ast.atom_expr -> Anf_ast.complex_expr) : Anf_ast.complex_expr =
      match expr with
      | Desugared_ast.Identifier (name, _ty) ->
        k (Anf_ast.Identifier name)
      | Desugared_ast.Integer i ->
        k (Anf_ast.Integer i)
      | Desugared_ast.Boolean b ->
        k (Anf_ast.Boolean b)
      | Desugared_ast.StringLiteral s ->
        k (Anf_ast.StringLiteral s)
      | Desugared_ast.Unit ->
        k (Anf_ast.Unit)
      | Desugared_ast.Lambda (params, expr, _ty) ->
        let f = NameGen.fresh "f" in
        let t = convert_expr expr mk_halt in
        Fun(f, params, t, k (Anf_ast.Identifier f))
      | Desugared_ast.Call (callee, args, _ty) ->
        let* f = convert_expr callee in
        let x = List.map (fun _ -> Anf_ast.Unit) args in
        (match f with
          | Anf_ast.Identifier f ->
            let r = NameGen.fresh "r" in
            Anf_ast.App(r, f, x, k (Anf_ast.Identifier r))
          | _ -> failwith "Must apply named value")
      | Desugared_ast.If (cond_expr, then_expr, else_expr, _ty) ->
        let* cond_expr = convert_expr cond_expr in
        let j, p = NameGen.fresh "j", NameGen.fresh "p" in
        let join v = Anf_ast.Jump(j, Some v) in
        Join(j, Some p, k (Anf_ast.Identifier p),
          If(cond_expr, convert_expr then_expr join, convert_expr else_expr join))
      | Desugared_ast.BinOp (op, lhs_expr, rhs_expr, _ty) ->
        let* lhs_expr = convert_expr lhs_expr in
        let* rhs_expr = convert_expr rhs_expr in
        let r = NameGen.fresh "r" in
        BinOp(r, op, lhs_expr, rhs_expr, k (Anf_ast.Identifier r))
      | Desugared_ast.UnOp (op, expr, _ty) ->
        let* expr = convert_expr expr in
        let r = NameGen.fresh "r" in
        UnOp(r, op, expr, k (Anf_ast.Identifier r))
      | Desugared_ast.LetIn (name, expr, body, _ty) ->
        let* expr = convert_expr expr in
        LetIn(NameGen.fresh name, expr, convert_expr body k)
      | _ -> failwith "Not implemented"
    in

    let anf_top_level_declaration: Desugared_ast.top_level_declaration -> Anf_ast.complex_expr = function
      | Desugared_ast.Let (name, expr, _ty) ->
        let* expr = convert_expr expr in
        LetIn(NameGen.fresh name, expr, mk_halt (Anf_ast.Identifier (NameGen.fresh name)))
    in

    let anf_program: Desugared_ast.program -> Anf_ast.program = function
      | Desugared_ast.Program (top_level_declarations, _ty) ->
        let decls = List.map anf_top_level_declaration top_level_declarations in
        Anf_ast.Program(decls)
    in

    anf_program
end

module Pprint_anf = struct
  let rec string_of_atom_expr = function
    | Anf_ast.Identifier name -> name
    | Anf_ast.Integer i -> string_of_int i
    | Anf_ast.Boolean b -> string_of_bool b
    | Anf_ast.StringLiteral s -> "\"" ^ s ^ "\""
    | Anf_ast.Unit -> "()"
  
  and string_of_complex_expr = function
    | Anf_ast.Halt expr -> "HALT " ^ string_of_atom_expr expr
    | Anf_ast.Fun (name, params, body, k) ->
      "fun " ^ name ^ " " ^ (String.concat " " (List.map (fun (name, ty) -> name ^ " : " ^ string_of_ty ty) params)) ^ " = " ^ string_of_complex_expr body ^ " in " ^ string_of_complex_expr k
    | Anf_ast.App (r, f, args, k) ->
      "App(" ^ r ^ ", " ^ f ^ ", [" ^ String.concat ", " (List.map string_of_atom_expr args) ^ "], " ^ string_of_complex_expr k ^ ")"
    | Anf_ast.Join (j, p, t, e) ->
      "Join(" ^ j ^ ", " ^ string_of_option p ^ ", " ^ string_of_complex_expr t ^ ", " ^ string_of_complex_expr e ^ ")"
    | Anf_ast.Jump (j, expr) ->
      "Jump(" ^ j ^ ", " ^ string_of_atom_expr_option expr ^ ")"
    | Anf_ast.BinOp (r, op, lhs, rhs, k) ->
      "BinOp(" ^ r ^ ", " ^ string_of_bin_op op ^ ", " ^ string_of_atom_expr lhs ^ ", " ^ string_of_atom_expr rhs ^ ", " ^ string_of_complex_expr k ^ ")"
    | Anf_ast.UnOp (r, op, expr, k) ->
      "UnOp(" ^ r ^ ", " ^ string_of_un_op op ^ ", " ^ string_of_atom_expr expr ^ ", " ^ string_of_complex_expr k ^ ")"
    | Anf_ast.If (cond_expr, then_expr, else_expr) ->
      "If(" ^ string_of_atom_expr cond_expr ^ ", " ^ string_of_complex_expr then_expr ^ ", " ^ string_of_complex_expr else_expr ^ ")"
    | Anf_ast.LetIn (name, expr, body) ->
      "LetIn(" ^ name ^ ", " ^ string_of_atom_expr expr ^ ", " ^ string_of_complex_expr body ^ ")"
    | _ -> failwith "Not implemented"

  and string_of_option = function
    | Some s -> s
    | None -> "None"

  and string_of_atom_expr_option = function
    | Some expr -> string_of_atom_expr expr
    | None -> "None"

  and string_of_program (Anf_ast.Program (decls)) =
    String.concat "\n" (List.map string_of_complex_expr decls)
end