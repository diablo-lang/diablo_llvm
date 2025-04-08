(* open Diablo.Compile *)

(* let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let () =
  (* Check if a filename is provided as the first argument *)
  if Array.length Sys.argv < 2 then
    (* No filename provided, so print an error and exit *)
    Printf.printf "Error: Please provide a filename as the first argument.\n"
  else
    let filename = Sys.argv.(1) in
    (* Get the filename from the first argument *)
    let s = read_file filename in
    (* Read the entire file contents *)
    compile s

 *)

open Typing.Expr
open Typing.Hm2

let core =
  let core_ref = ref Env.empty in
  let assume name ty =
    core_ref := Env.extend !core_ref name ty
  in

  assume "one" (TConst "int");
  assume "zero" (TConst "int");
  assume "add" (TArrow([TConst "int"; TConst "int"], TConst "int"));
  assume "sub" (TArrow([TConst "int"; TConst "int"], TConst "int"));
  assume "false" (TConst "bool");
  assume "true" (TConst "bool");
  assume "and" (TArrow([TConst "bool"; TConst "bool"], TConst "bool"));
  assume "or" (TArrow([TConst "bool"; TConst "bool"], TConst "bool"));
  assume "not" (TArrow([TConst "bool"], TConst "bool"));
  assume "eq" (TArrow([TConst "int"; TConst "int"], TConst "bool"));
  assume "neq" (TArrow([TConst "int"; TConst "int"], TConst "bool"));
  assume "lt" (TArrow([TConst "int"; TConst "int"], TConst "bool"));
  assume "leq" (TArrow([TConst "int"; TConst "int"], TConst "bool"));
  assume "gt" (TArrow([TConst "int"; TConst "int"], TConst "bool"));

!core_ref

let example_expr =
  Fun([("x", TConst "int"); ("y", TConst "int")],
      Let("z", Call(Identifier "add", [Identifier "x"; Identifier "y"]), Identifier "z"),
      TConst "int")

let example_two =
  Fun([("x", TConst "int")],
      Let("x", Identifier "true", Identifier "x"),
      TConst "int")


let if_statement_example_expr =
  If(Boolean true, Integer 1, Integer 2)

let if_statement_example_expr2 =
  If(Boolean false, Integer 3, Integer 2)

let complex_function_example_expr =
  Fun([("x", TConst "int"); ("y", TConst "int")],
      Let("z", Call(Identifier "add", [Identifier "x"; Identifier "y"]), If(Boolean true, Identifier "x", Identifier "z")),
      TConst "int")

let complex_list_example_expr =
  List([Integer 1; Integer 2; Integer 3])

let complex_record_example_expr =
  Record([("x", Integer 1); ("y", Integer 2)])

let custom_person_type_example_expr =
  Record([("name", StringLiteral "John"); ("age", Integer 30)])

let typecheck core expr =
  let ty = infer core 0 expr in
  let gty = generalize (-1) ty in
  string_of_ty gty

let () =
  let ty = typecheck core example_expr in
  Printf.printf "Example expr 1: %s\n" ty;

  let ty2 = typecheck core example_two in
  Printf.printf "Example expr 2: %s\n" ty2;

  let ty3 = typecheck core if_statement_example_expr in
  Printf.printf "Example expr 3: %s\n" ty3;

  let ty4 = typecheck core if_statement_example_expr2 in
  Printf.printf "Example expr 4: %s\n" ty4;

  let ty5 = typecheck core complex_function_example_expr in
  Printf.printf "Example expr 5: %s\n" ty5;

  let ty6 = typecheck core complex_list_example_expr in
  Printf.printf "Example expr 6: %s\n" ty6;

  let ty7 = typecheck core complex_record_example_expr in
  Printf.printf "Example expr 7: %s\n" ty7;
