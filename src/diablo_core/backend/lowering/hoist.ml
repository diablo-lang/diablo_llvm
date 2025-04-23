(* open Anf
open Ast.Ast_types
open Alpha

module Hoist = struct
  
type join = name * name option * Anf_ast.complex_expr
type func = name * name list * join * join list
type 'a dlist = 'a list -> 'a list
let cons x xs = x :: xs
let (@-) xs ys tl = xs (ys tl)
let hoist e : func list =
  let rec go : Anf_ast.complex_expr -> func dlist * join dlist * Anf_ast.complex_expr = function
  | Fun (f, xs, e, e') ->
    let (fs, js, e) = go e in
    let (fs', js', e') = go e' in
    let entry = fresh "entry" in
    let fn = (f, xs, (entry, None, e), js []) in
    (fs @- fs' @- cons fn, js', e')
  | Join (j, p, e, e') ->
    let (fs, js, e) = go e in
    let (fs', js', e') = go e' in
    let jn = (j, p, e) in
    (fs @- fs', cons jn @- js @- js', e')
  | If (e, t, f) ->
    let (fs, js, t) = go t in
    let (fs', js', f) = go f in
    let th, el = fresh "then", fresh "else" in
    let bt, bf = (th, None, t), (el, None, f) in
    (fs @- fs', cons bt @- cons bf @- js @- js',
        If (e, Jump (th, None), Jump (el, None)))
  | _ -> failwith "Not implemented"
  in
  let (fs, js, e) = go e in
  List.rev fs
end *)
