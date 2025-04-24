open Alcotest
open Desugaring.Desugared_ast
open Lowering.Fresh
open Lowering.Anf
open Ast.Ast_types

let test_simple_anf_conversion () =
  NameGen.reset ();
  let ast = Program ([ Let ("x", Integer 42, TConst "int") ], TConst "unit") in
  let result = anf_of_program ast in
  match result with
  | ANFProgram [ ("x", ANFInt 42) ] -> ()
  | _ ->
      print_endline (string_of_anf_program result);
      print_endline (string_of_anf_program (ANFProgram [ ("x", ANFInt 42) ]));
      Alcotest.fail "Failed simple let test"

let tests = [ test_case "simple let" `Quick test_simple_anf_conversion ]
