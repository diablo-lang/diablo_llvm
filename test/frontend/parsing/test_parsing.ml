open Alcotest
open Ast.Ast_types
open Parsing
open Parsing.Parsed_ast

let pp_program fmt prog =
  Format.fprintf fmt "%s" (Pprint_past.string_of_program prog)

let pp_module fmt module_file =
  Format.fprintf fmt "%s" (Pprint_past.string_of_module module_file)

let program_tests = [
  test_case "Parse empty program" `Quick (fun () ->
    let prog_str = "" in
    let expected = Program ([], []) in
    let lexbuf = Lexing.from_string prog_str in
    let result = Parser.program Lexer.read_token lexbuf in
    check (testable pp_program (=)) "program" expected result
  );

  test_case "Parse simple program" `Quick (fun () ->
    let prog_str = "x = 1;" in
    let expected = Program ([], [Let ("x", Integer 1)]) in
    let lexbuf = Lexing.from_string prog_str in
    let result = Parser.program Lexer.read_token lexbuf in
    check (testable pp_program (=)) "program" expected result
  );

  test_case "Parse function" `Quick (fun () ->
    let prog_str = "
    fn foo (x: int) -> int {
      return x
    }
    x = foo(1);
    " in
    let expected = Program ([], [
      Function ("foo", [("x", TConst "int")], Identifier ("x"), TConst "int");
      Let ("x", Call (Identifier ("foo"), [Integer 1]))
    ]) in
    let lexbuf = Lexing.from_string prog_str in
    let result = Parser.program Lexer.read_token lexbuf in
    check (testable pp_program (=)) "program" expected result
  );

  test_case "Parse if statement" `Quick (fun () ->
    let prog_str = "x = if (1 > 2) { 1 } else { 2 };" in
    let expected = Program ([], [
      Let ("x", If (BinOp (BinOpGreaterThan, Integer 1, Integer 2), Integer 1, Integer 2))
    ]) in
    let lexbuf = Lexing.from_string prog_str in
    let result = Parser.program Lexer.read_token lexbuf in
    check (testable pp_program (=)) "program" expected result
  );

  test_case "Parse lambda" `Quick (fun () ->
    let prog_str = "
    x = fn (x: int) -> int {
      return x
    };
    " in
    let expected = Program ([], [
      Let ("x", Lambda ([("x", TConst "int")], Identifier ("x"), TConst "int"));
    ]) in
    let lexbuf = Lexing.from_string prog_str in
    let result = Parser.program Lexer.read_token lexbuf in
    check (testable pp_program (=)) "program" expected result
  );

  test_case "Parse return" `Quick (fun () ->
    let prog_str = "
    fn foo (a: int, b: int) -> int {
      c = a + b;
      d = c / 2;
      return d * 1
    }
    x = foo(1, 2);
    " in
    let expected = Program ([], [
      Function ("foo", [("a", TConst "int"); ("b", TConst "int")],
      LetIn ("c", BinOp (BinOpPlus, Identifier ("a"), Identifier ("b")),
        LetIn ("d", BinOp (BinOpDiv, Identifier ("c"), Integer 2),
        BinOp (BinOpMult, Identifier ("d"), Integer 1)))
      , TConst "int");
      Let ("x", Call (Identifier ("foo"), [Integer 1; Integer 2]))
    ]) in
    let lexbuf = Lexing.from_string prog_str in
    let result = Parser.program Lexer.read_token lexbuf in
    check (testable pp_program (=)) "program" expected result
  );

  test_case "Parse list" `Quick (fun () ->
    let prog_str = "
    x = [1, 2, 3];
    " in
    let expected = Program ([], [
      Let ("x", List [Integer 1; Integer 2; Integer 3])
    ]) in
    let lexbuf = Lexing.from_string prog_str in
    let result = Parser.program Lexer.read_token lexbuf in
    check (testable pp_program (=)) "program" expected result
  );

  test_case "Parse import" `Quick (fun () ->
    let prog_str = "
    import foo;
    " in
    let expected = Program ([
      Import "foo"
    ], []) in
    let lexbuf = Lexing.from_string prog_str in
    let result = Parser.program Lexer.read_token lexbuf in
    check (testable pp_program (=)) "program" expected result
  );

  test_case "Parse module" `Quick (fun () ->
    let prog_str = "
    import bar;
    module foo {
      fn foo (x: int) -> int {
        return x
      }
    }
    " in
    let expected = Module ([
      Import "bar"
    ], ModuleDefinition ("foo", [
      Function ("foo", [("x", TConst "int")], Identifier ("x"), TConst "int")
    ])) in
    let lexbuf = Lexing.from_string prog_str in
    let result = Parser.module_file Lexer.read_token lexbuf in
    check (testable pp_module (=)) "module" expected result
  );
]

let () =
  Alcotest.run "Parser Tests" [
    "Programs", program_tests;
  ]
