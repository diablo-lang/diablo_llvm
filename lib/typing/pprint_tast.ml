open Ast.Ast_types

let pprint_type_env env =
  let bindings = List.map (fun (x, t) -> Printf.sprintf "%s: %s" x (Type.to_string t)) env in
  String.concat ", " bindings

let pprint_type t =
  Type.to_string t