let () =
  Alcotest.run "Lowering Tests" [
    "Alpha", Test_alpha.tests;
    "ANF", Test_anf.tests;
  ]
