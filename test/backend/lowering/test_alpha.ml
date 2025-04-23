open Alcotest
open Ast.Ast_types
open Desugaring.Desugared_ast
open Lowering.Alpha
open Lowering.Fresh

let int_ty = TConst "int"

let rec pp_program ppf (Program (stmts, _)) =
  let pp_stmt ppf = function
    | Let (name, expr, _) -> Fmt.pf ppf "Let %s = %a" name pp_expr expr
  in
  Fmt.pf ppf "%a" (Fmt.list pp_stmt) stmts

and pp_expr ppf = function
  | Identifier (name, _) -> Fmt.pf ppf "%s" name
  | Integer i -> Fmt.pf ppf "%d" i
  | Boolean b -> Fmt.pf ppf "%b" b
  | StringLiteral s -> Fmt.pf ppf "\"%s\"" s
  | Unit -> Fmt.pf ppf "()"
  | Call (callee, args, _) ->
      Fmt.pf ppf "%a(%a)" pp_expr callee (Fmt.list pp_expr) args
  | Lambda (params, body, _) ->
      Fmt.pf ppf "\\%a -> %a" (Fmt.list (fun ppf (name, _) -> Fmt.pf ppf "%s" name)) params pp_expr body
  | LetIn (name, expr, body, _) ->
      Fmt.pf ppf "let %s = %a in %a" name pp_expr expr pp_expr body
  | BinOp (op, lhs, rhs, _) ->
      Fmt.pf ppf "%a %s %a" pp_expr lhs (string_of_bin_op op) pp_expr rhs
  | UnOp (op, expr, _) ->
      Fmt.pf ppf "%s%a" (string_of_un_op op) pp_expr expr
  | List (exprs, _) ->
      Fmt.pf ppf "[%a]" (Fmt.list pp_expr) exprs
  | If (cond_expr, then_expr, else_expr, _) ->
      Fmt.pf ppf "if %a then %a else %a" pp_expr cond_expr pp_expr then_expr pp_expr else_expr

let test_simple_let () =
  NameGen.reset ();
  let program =
    Program ([
      Let ("x", Integer 42, int_ty);
      Let ("y", Identifier ("x", int_ty), int_ty)
    ], int_ty)
  in
  let expected =
    Program ([
      Let ("x0", Integer 42, int_ty);
      Let ("y1", Identifier ("x0", int_ty), int_ty)
    ], int_ty)
  in
  check (testable pp_program (=)) "Simple let renaming" expected (Alpha.rename program)

let test_lambda_shadowing () =
  NameGen.reset ();
  let program =
    Program ([
      Let ("f",
        Lambda ([("x", int_ty)],
          LetIn ("x", Integer 1, Identifier ("x", int_ty), int_ty),
          TArrow ([int_ty], int_ty)
        ),
        TArrow ([int_ty], int_ty)
      )
    ], int_ty)
  in
  let expected =
    Program ([
      Let ("f0",
        Lambda ([("x1", int_ty)],
          LetIn ("x2", Integer 1, Identifier ("x2", int_ty), int_ty),
          TArrow ([int_ty], int_ty)
        ),
        TArrow ([int_ty], int_ty)
      )
    ], int_ty)
  in
  check (testable pp_program (=)) "Lambda shadowing" expected (Alpha.rename program)

let test_lambda_reference_outside () =
  NameGen.reset ();
  let program =
    Program ([
      Let ("a", Integer 1, int_ty);
      Let ("f",
        Lambda ([("x", int_ty)],
          Identifier ("a", int_ty),
          TArrow ([int_ty], int_ty)
        ),
        TArrow ([int_ty], int_ty)
      )
    ], int_ty)
  in
  let expected =
    Program ([
      Let ("a0", Integer 1, int_ty);
      Let ("f1",
        Lambda ([("x2", int_ty)],
          Identifier ("a0", int_ty),
          TArrow ([int_ty], int_ty)
        ),
        TArrow ([int_ty], int_ty)
      )
    ], int_ty)
  in
  check (testable pp_program (=)) "Lambda referencing outer variable" expected (Alpha.rename program)

let tests = [
  test_case "simple let" `Quick test_simple_let;
  test_case "lambda shadowing" `Quick test_lambda_shadowing;
  test_case "lambda references outer var" `Quick test_lambda_reference_outside;
]
