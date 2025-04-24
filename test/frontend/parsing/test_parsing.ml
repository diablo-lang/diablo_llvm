open Alcotest
open Ast.Ast_types
open Parsing
open Parsing.Parsed_ast

let pp_program fmt prog =
  Format.fprintf fmt "%s" (Pprint_past.string_of_program prog)

let pp_module fmt module_file =
  Format.fprintf fmt "%s" (Pprint_past.string_of_module module_file)

let program_tests =
  [
    test_case "Parse empty program" `Quick (fun () ->
        let prog_str = "" in
        let expected = Program ([], []) in
        let lexbuf = Lexing.from_string prog_str in
        let result = Parser.program Lexer.read_token lexbuf in
        check (testable pp_program ( = )) "program" expected result);
    test_case "Parse simple program" `Quick (fun () ->
        let prog_str = "x = 1;" in
        let expected = Program ([], [ Let ("x", Integer 1) ]) in
        let lexbuf = Lexing.from_string prog_str in
        let result = Parser.program Lexer.read_token lexbuf in
        check (testable pp_program ( = )) "program" expected result);
    test_case "Parse function" `Quick (fun () ->
        let prog_str =
          "\n\
          \    fn foo (x: int) -> int {\n\
          \      return x\n\
          \    }\n\
          \    x = foo(1);\n\
          \    "
        in
        let expected =
          Program
            ( [],
              [
                Function
                  ("foo", [ ("x", TConst "int") ], Identifier "x", TConst "int");
                Let ("x", Call (Identifier "foo", [ Integer 1 ]));
              ] )
        in
        let lexbuf = Lexing.from_string prog_str in
        let result = Parser.program Lexer.read_token lexbuf in
        check (testable pp_program ( = )) "program" expected result);
    test_case "Parse if statement" `Quick (fun () ->
        let prog_str = "x = if (1 > 2) { 1 } else { 2 };" in
        let expected =
          Program
            ( [],
              [
                Let
                  ( "x",
                    If
                      ( BinOp (BinOpGreaterThan, Integer 1, Integer 2),
                        Integer 1,
                        Integer 2 ) );
              ] )
        in
        let lexbuf = Lexing.from_string prog_str in
        let result = Parser.program Lexer.read_token lexbuf in
        check (testable pp_program ( = )) "program" expected result);
    test_case "Parse lambda" `Quick (fun () ->
        let prog_str =
          "\n    x = fn (x: int) -> int {\n      return x\n    };\n    "
        in
        let expected =
          Program
            ( [],
              [
                Let
                  ( "x",
                    Lambda
                      ([ ("x", TConst "int") ], Identifier "x", TConst "int") );
              ] )
        in
        let lexbuf = Lexing.from_string prog_str in
        let result = Parser.program Lexer.read_token lexbuf in
        check (testable pp_program ( = )) "program" expected result);
    test_case "Parse return" `Quick (fun () ->
        let prog_str =
          "\n\
          \    fn foo (a: int, b: int) -> int {\n\
          \      c = a + b;\n\
          \      d = c / 2;\n\
          \      return d * 1\n\
          \    }\n\
          \    x = foo(1, 2);\n\
          \    "
        in
        let expected =
          Program
            ( [],
              [
                Function
                  ( "foo",
                    [ ("a", TConst "int"); ("b", TConst "int") ],
                    LetIn
                      ( "c",
                        BinOp (BinOpPlus, Identifier "a", Identifier "b"),
                        LetIn
                          ( "d",
                            BinOp (BinOpDiv, Identifier "c", Integer 2),
                            BinOp (BinOpMult, Identifier "d", Integer 1) ) ),
                    TConst "int" );
                Let ("x", Call (Identifier "foo", [ Integer 1; Integer 2 ]));
              ] )
        in
        let lexbuf = Lexing.from_string prog_str in
        let result = Parser.program Lexer.read_token lexbuf in
        check (testable pp_program ( = )) "program" expected result);
    test_case "Parse list" `Quick (fun () ->
        let prog_str = "\n    x = [1, 2, 3];\n    " in
        let expected =
          Program ([], [ Let ("x", List [ Integer 1; Integer 2; Integer 3 ]) ])
        in
        let lexbuf = Lexing.from_string prog_str in
        let result = Parser.program Lexer.read_token lexbuf in
        check (testable pp_program ( = )) "program" expected result);
    test_case "Parse import" `Quick (fun () ->
        let prog_str = "\n    import foo;\n    " in
        let expected = Program ([ Import "foo" ], []) in
        let lexbuf = Lexing.from_string prog_str in
        let result = Parser.program Lexer.read_token lexbuf in
        check (testable pp_program ( = )) "program" expected result);
    test_case "Parse module" `Quick (fun () ->
        let prog_str =
          "\n\
          \    import bar;\n\
          \    module foo {\n\
          \      fn foo (x: int) -> int {\n\
          \        return x\n\
          \      }\n\
          \    }\n\
          \    "
        in
        let expected =
          Module
            ( [ Import "bar" ],
              ModuleDefinition
                ( "foo",
                  [
                    Function
                      ( "foo",
                        [ ("x", TConst "int") ],
                        Identifier "x",
                        TConst "int" );
                  ] ) )
        in
        let lexbuf = Lexing.from_string prog_str in
        let result = Parser.module_file Lexer.read_token lexbuf in
        check (testable pp_module ( = )) "module" expected result);
  ]

let () = Alcotest.run "Parser Tests" [ ("Programs", program_tests) ]
