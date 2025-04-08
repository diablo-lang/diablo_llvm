open Lexing
open Parsing
open Resolution
open Typing
(* open Desugaring *)
(* open Lowering *)
open Ir_gen

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  Printf.sprintf "line %d, column %d" line col

let compile (s : string) =
  let lexbuf = Lexing.from_string s in
  print_endline "[*] Parsing...";
  try
    let parsed_ast = Parser.program Lexer.read_token lexbuf in
    print_endline (Pprint_past.pprint_program parsed_ast);

    print_endline "[*] Resolving imports...";
    let resolved_ast = Resolve.resolve_imports parsed_ast in
    print_endline (Pprint_past.pprint_program resolved_ast);

    print_endline "[*] Typechecking...";
    let program_type = Hm.type_check resolved_ast in
    let typed_ast = Hm.convert_to_typed_ast resolved_ast in
    print_endline (Pprint_tast.pprint_program typed_ast);
    print_endline (Pprint_tast.pprint_type program_type);

    print_endline "[*] Desugaring...";
    (* let desugared_ast = Desugaring.desugar_program typed_ast in *)
    (* print_endline (Pprint_dast.pprint_program desugared_ast); *)

    print_endline "[*] Lowering to MIR...";
    (* let lowered_ast = Lowering.lower_program desugared_ast in *)
    (* print_endline (Pprint_mir.pprint_program lowered_ast); *)

    print_endline "[*] Generating LLVM IR...";
    (* Llvm_ir_gen.Codegen.codegen_program lowered_ast; *)
    (* Llvm_ir_gen.Codegen.print_module_to_stderr (); *)
    (* Llvm_ir_gen.Codegen.save_module_to_file "llvm_bin/output.ll"; *)

    print_endline "[*] Done!"
  with
  | Lexer.SyntaxError msg ->
      Printf.eprintf "Lexer error at %s: %s\n" (print_position lexbuf) msg
  | Parser.Error ->
      Printf.eprintf "Parser error at %s\n" (print_position lexbuf)
  | Hm.TypeError msg -> Printf.eprintf "Type error at: %s\n" msg
  | Llvm_ir_gen.Codegen.LLVMError msg -> Printf.eprintf "LLVM error: %s\n" msg
