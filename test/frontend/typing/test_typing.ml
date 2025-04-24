open Alcotest
open Ast.Ast_types
open Parsing.Parsed_ast
open Typing
open Typing.Hm

let t_int = TConst "int"

let test_integer_literal () =
  let parsed = Program ([], [ Let ("x", Integer 42) ]) in
  let typed = annotate_program parsed in
  match typed with
  | Typed_ast.Program ([ Let ("x", Integer 42, TConst "int") ], TConst "unit")
    ->
      ()
  | _ -> Alcotest.fail "Failed integer literal test"

let test_boolean_literal () =
  let parsed = Program ([], [ Let ("flag", Boolean true) ]) in
  let typed = annotate_program parsed in
  match typed with
  | Typed_ast.Program
      ([ Let ("flag", Boolean true, TConst "bool") ], TConst "unit") ->
      ()
  | _ -> Alcotest.fail "Failed boolean literal test"

let test_simple_function () =
  let parsed =
    Program ([], [ Function ("id", [ ("x", t_int) ], Identifier "x", t_int) ])
  in
  let typed = annotate_program parsed in
  match typed with
  | Typed_ast.Program
      ( [
          Function
            ( "id",
              [ ("x", TConst "int") ],
              Identifier ("x", TConst "int"),
              TConst "int" );
        ],
        TConst "unit" ) ->
      ()
  | _ -> Alcotest.fail "Failed simple function test"

let test_complex_function () =
  let parsed =
    Program
      ( [],
        [
          Function
            ( "add",
              [ ("x", t_int); ("y", t_int) ],
              BinOp (BinOpPlus, Identifier "x", Identifier "y"),
              t_int );
          Let ("x", Call (Identifier "add", [ Integer 1; Integer 2 ]));
        ] )
  in
  let typed = annotate_program parsed in
  match typed with
  | Typed_ast.Program
      ( [
          Function
            ( "add",
              [ ("x", TConst "int"); ("y", TConst "int") ],
              BinOp
                ( BinOpPlus,
                  Identifier ("x", TConst "int"),
                  Identifier ("y", TConst "int"),
                  TConst "int" ),
              TConst "int" );
          Let
            ( "x",
              Call
                ( Identifier
                    ( "add",
                      TArrow ([ TConst "int"; TConst "int" ], TConst "int") ),
                  [ Integer 1; Integer 2 ],
                  TConst "int" ),
              TConst "int" );
        ],
        TConst "unit" ) ->
      ()
  | _ -> Alcotest.fail "Failed complex function test"

let _test_shadowing () =
  let parsed =
    Program
      ( [],
        [
          Let ("x", Integer 42);
          Function
            ("foo", [ ("x", TConst "bool") ], Identifier "x", TConst "bool");
          Function ("bar", [ ("a", TConst "int") ], Identifier "x", TConst "int");
          Let ("r1", Call (Identifier "foo", [ Boolean true ]));
          Let ("r2", Call (Identifier "bar", [ Integer 1 ]));
        ] )
  in
  let typed = annotate_program parsed in
  match typed with
  | Typed_ast.Program
      ( [
          Let ("x", Integer 42, TConst "int");
          Function
            ( "foo",
              [ ("x", TConst "bool") ],
              Identifier ("x", TConst "bool"),
              TConst "bool" );
          Function
            ( "bar",
              [ ("a", TConst "int") ],
              Identifier ("x", TConst "int"),
              TConst "int" );
          Let
            ( "r1",
              Call
                ( Identifier ("foo", TArrow ([ TConst "bool" ], TConst "bool")),
                  [ Boolean true ],
                  TConst "bool" ),
              TConst "bool" );
          Let
            ( "r2",
              Call
                ( Identifier ("bar", TArrow ([ TConst "int" ], TConst "int")),
                  [ Integer 1 ],
                  TConst "int" ),
              TConst "int" );
        ],
        TConst "unit" ) ->
      ()
  | _ -> Alcotest.fail "Failed shadowing test"

let () =
  run "Typechecker Tests"
    [
      ( "Basic Expressions",
        [
          test_case "Integer literal" `Quick test_integer_literal;
          test_case "Boolean literal" `Quick test_boolean_literal;
          test_case "Simple function" `Quick test_simple_function;
          test_case "Complex function" `Quick test_complex_function;
          (* TODO: Fix x's type being overriden as bool due to shadowing (same name) *)
          (* test_case "Shadowing" `Quick test_shadowing; *)
        ] );
    ]
