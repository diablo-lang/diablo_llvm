open Ast.Ast_types
open Parsing.Parsed_ast
open Typing
open Typing.Hm

let t_int = TConst "int"

let test_integer_literal () =
  let parsed = Program([], [ Let ("x", Integer 42) ]) in
  let typed = annotate_program parsed in
  match typed with
  | Typed_ast.Program ([ Let ("x", Integer (42), TConst "int") ], TConst "unit") -> ()
  | _ -> Alcotest.fail "Failed integer literal test"


let test_boolean_literal () =
  let parsed = Program([], [ Let ("flag", Boolean true) ]) in
  let typed = annotate_program parsed in
  match typed with
  | Typed_ast.Program ([ Let ("flag", Boolean (true), TConst "bool") ], TConst "unit") -> ()
  | _ -> Alcotest.fail "Failed boolean literal test"


let test_simple_function () =
  let parsed =
    Program(
      [],
      [ Function ("id", [ ("x", t_int) ], Identifier "x", t_int) ]
    )
  in
  let typed = annotate_program parsed in
  match typed with
  | Typed_ast.Program (
      [ Function ("id", [ ("x", TConst "int") ], Identifier ("x", TConst "int"), TConst "int") ],
      TConst "unit"
    ) -> ()
  | _ -> Alcotest.fail "Failed simple function test"

let test_complex_function () =
  let parsed =
    Program(
      [],
      [
        Function ("add", [ ("x", t_int); ("y", t_int) ], BinOp (BinOpPlus, Identifier "x", Identifier "y"), t_int);
        Let ("x", Call (Identifier "add", [Integer 1; Integer 2]));
      ]
    )
  in
  let typed = annotate_program parsed in
  match typed with
  | Typed_ast.Program (
      [
        Function ("add", [ ("x", TConst "int"); ("y", TConst "int") ],
          BinOp (BinOpPlus, Identifier ("x", TConst "int"), Identifier ("y", TConst "int"), TConst "int"), TConst "int");
        Let ("x", Call (Identifier ("add", TArrow([TConst "int"; TConst "int"], TConst "int")), [Integer 1; Integer 2], TConst "int"), TConst "int");
      ],
      TConst "unit"
    ) -> ()
  | _ ->
    print_endline (Pprint_tast.string_of_program typed);
    Alcotest.fail "Failed simple function test"

let () =
  let open Alcotest in
  run "Typechecker Tests"
    [ ( "Basic Expressions",
        [ test_case "Integer literal" `Quick test_integer_literal;
          test_case "Boolean literal" `Quick test_boolean_literal;
          test_case "Simple function" `Quick test_simple_function;
          test_case "Complex function" `Quick test_complex_function;
        ] )
    ]
