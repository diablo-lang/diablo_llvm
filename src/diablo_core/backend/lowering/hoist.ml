open Ast.Ast_types
open Anf
open Fresh

type join = name * name option * cexpr
type func = name * name list * join * join list
type 'a dlist = 'a list -> 'a list

let cons x xs = x :: xs
let ( @- ) xs ys tl = xs (ys tl)

let hoist e : func list =
  let rec go : cexpr -> func dlist * join dlist * cexpr = function
    | Fun (f, xs, e, e') ->
        let fs, js, e = go e in
        let fs', js', e' = go e' in
        let entry = NameGen.fresh "entry" in
        let fn = (f, xs, (entry, None, e), js []) in
        (fs @- fs' @- cons fn, js', e')
    | Join (j, p, e, e') ->
        let fs, js, e = go e in
        let fs', js', e' = go e' in
        let jn = (j, p, e) in
        (fs @- fs', cons jn @- js @- js', e')
    | If (e, t, f) ->
        let fs, js, t = go t in
        let fs', js', f = go f in
        let th, el = (NameGen.fresh "then", NameGen.fresh "else") in
        let bt, bf = ((th, None, t), (el, None, f)) in
        ( fs @- fs',
          cons bt @- cons bf @- js @- js',
          If (e, Jump (th, None), Jump (el, None)) )
    | Halt atom -> ((fun fs -> fs), (fun js -> js), Halt atom)
    | Let (name, value, body) ->
        let fs, js, body = go body in
        (fs, js, Let (name, value, body))
    | Tuple (name, args, body) ->
        let fs, js, body = go body in
        (fs, js, Tuple (name, args, body))
    | Proj (name, x, i, body) ->
        let fs, js, body = go body in
        (fs, js, Proj (name, x, i, body))
    | BinOp (r, op, lhs, rhs, e) ->
        let fs, js, e = go e in
        (fs, js, BinOp (r, op, lhs, rhs, e))
    | UnOp (r, op, expr, e) ->
        let fs, js, e = go e in
        (fs, js, UnOp (r, op, expr, e))
    | App (r, f, args, e) ->
        let fs, js, e = go e in
        (fs, js, App (r, f, args, e))
    | Jump (name, atom) -> ((fun fs -> fs), (fun js -> js), Jump (name, atom))
  in
  let fs, _js, _e' = go e in
  fs []

let hoist_program (l : (string * cexpr) list) =
  List.map (fun (x, e) -> (x, hoist e)) l

let pp_func (f, xs, (e, p, _), js) =
  "Function " ^ f ^ " (" ^ String.concat ", " xs ^ ") {\n  " ^ "entry = " ^ e
  ^ "\n  "
  ^ (match p with Some p -> "param = " ^ p ^ "\n  " | None -> "")
  ^ String.concat "\n  "
      (List.map (fun (j, _, e) -> "join " ^ j ^ " = " ^ pp_cexpr e) js)
  ^ "\n}\n"

let pp_funcs fs = String.concat "\n" (List.map pp_func fs)

let pp_program : (string * func list) list -> string =
 fun fs ->
  String.concat "\n" (List.map (fun (x, fs) -> x ^ " = " ^ pp_funcs fs) fs)

let string_of_option f = function None -> "" | Some x -> f x
let string_of_list f sep xs = String.concat sep (List.map f xs)
let indent s = "  " ^ s

(* Stub for string_of_cexpr. You can replace it with a real implementation. *)
let rec string_of_cexpr = function
  | Halt a -> "Halt " ^ string_of_aexpr a
  | Jump (n, Some a) -> "Jump " ^ n ^ " " ^ string_of_aexpr a
  | Jump (n, None) -> "Jump " ^ n
  | Let (n, x, e) ->
      Printf.sprintf "Let %s = %s in\n%s" n (string_of_aexpr x)
        (string_of_cexpr e)
  | Tuple (n, xs, e) ->
      Printf.sprintf "Tuple %s = (%s) in\n%s" n
        (String.concat ", " (List.map string_of_aexpr xs))
        (string_of_cexpr e)
  | Proj (n, x, i, e) ->
      Printf.sprintf "Proj %s = %s[%d] in\n%s" n x i (string_of_cexpr e)
  | BinOp (r, op, l, r', e) ->
      Printf.sprintf "BinOp %s = %s %s %s in\n%s" r (string_of_aexpr l)
        (string_of_bin_op op) (string_of_aexpr r') (string_of_cexpr e)
  | UnOp (r, op, x, e) ->
      Printf.sprintf "UnOp %s = %s %s in\n%s" r (string_of_un_op op)
        (string_of_aexpr x) (string_of_cexpr e)
  | App (r, f, args, e) ->
      Printf.sprintf "App %s = %s(%s) in\n%s" r f
        (String.concat ", " (List.map string_of_aexpr args))
        (string_of_cexpr e)
  | If (cond, t, f) ->
      Printf.sprintf "If %s then\n%s\nelse\n%s" (string_of_aexpr cond)
        (string_of_cexpr t) (string_of_cexpr f)
  | Fun _ | Join _ ->
      "<nested fun/join>" (* shouldn't appear directly after hoisting *)

and string_of_aexpr = function
  | AIdentifier x -> x
  | AInteger n -> string_of_int n
  | ABoolean b -> string_of_bool b
  | AStringLiteral s -> "\"" ^ s ^ "\""
  | AUnit -> "()"
  | AGlobal x -> x

let string_of_join (name, arg_opt, expr) =
  let args =
    match arg_opt with Some arg -> Printf.sprintf "(%s)" arg | None -> ""
  in
  Printf.sprintf "%s%s:\n%s" name args (indent (string_of_cexpr expr))

let string_of_func (fname, params, entry, joins) =
  let param_str = String.concat " " params in
  let entry_str = string_of_join entry in
  let joins_str =
    joins |> List.map string_of_join |> List.map indent |> String.concat "\n"
  in
  Printf.sprintf "fun %s %s:\n%s\n%s\n" fname param_str entry_str joins_str

let string_of_program : (string * func list) list -> string =
 fun fs ->
  String.concat "\n"
    (List.map
       (fun (x, fs) ->
         x ^ " = " ^ String.concat "\n" (List.map string_of_func fs))
       fs)
