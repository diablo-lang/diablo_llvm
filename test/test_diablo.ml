open Parsing
open Ast.Ast_types
open Typing

let print_typed_ast ast =
  match ast with
  | Typed_ast.Program (_import_stms, _funcs, Typed_ast.Block (ty, _)) ->
      Printf.sprintf "Program return type: %s" (Type.to_string ty)

(* Test case 1: Simple integer expression *)
let test_integer_expr () =
  let parsed_ast =
    Parsed_ast.Program ([], [], Parsed_ast.Block [ Parsed_ast.Integer 42 ])
  in
  let typed_ast = Hm.convert_to_typed_ast parsed_ast in
  Alcotest.(check string)
    "Integer type check" "Program return type: int"
    (print_typed_ast typed_ast)

(* Test case 2: Simple function definition *)
let test_function () =
  let parsed_ast =
    Parsed_ast.Program
      ( [],
        [
          Parsed_ast.TFunction
            ( "add",
              [ TParam (Type.TInt, "x"); TParam (Type.TInt, "y") ],
              Type.TInt,
              Parsed_ast.Block
                [
                  Parsed_ast.BinOp
                    ( BinOpPlus,
                      Parsed_ast.Identifier "x",
                      Parsed_ast.Identifier "y" );
                ] );
        ],
        Parsed_ast.Block [] )
  in
  let typed_ast = Hm.convert_to_typed_ast parsed_ast in
  Alcotest.(check string)
    "Function type check" "Program return type: void"
    (print_typed_ast typed_ast)

(* Test case 3: Simple if statement *)
let test_if () =
  let parsed_ast =
    Parsed_ast.Program
      ( [],
        [],
        Parsed_ast.Block
          [
            Parsed_ast.If
              (Parsed_ast.Boolean true, Parsed_ast.Block [], Parsed_ast.Block []);
          ] )
  in
  let typed_ast = Hm.convert_to_typed_ast parsed_ast in
  Alcotest.(check string)
    "If type check" "Program return type: void"
    (print_typed_ast typed_ast)

(* Run all tests *)
let () =
  let open Alcotest in
  run "Type Checker Tests"
    [
      ( "AST Type Inference",
        [
          test_case "Integer Expression" `Quick test_integer_expr;
          test_case "Function Definition" `Quick test_function;
          test_case "If Statement" `Quick test_if;
        ] );
      ("AST Type Checking", []);
    ]
