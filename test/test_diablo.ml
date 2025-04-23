(* Run all tests *)
let () =
  let open Alcotest in
  run "Type Checker Tests"
    [
      ( "AST Type Inference",
        [
        ] );
      ("AST Type Checking", []);
    ]
