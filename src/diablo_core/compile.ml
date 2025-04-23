open Lexing
open Parsing
open Resolution
open Typing
open Desugaring
open Lowering
(* open Ir_gen *)

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  Printf.sprintf "line %d, column %d" line col

let compile source verbose =
  print_endline "[*] Lexing...";
  let lexbuf = Lexing.from_string source in
  try

    print_endline "[*] Parsing...";
    let parsed_ast = Parser.program Lexer.read_token lexbuf in
    if verbose then print_endline (Pprint_past.string_of_program parsed_ast);

    print_endline "[*] Resolving imports...";
    let resolved_ast = Resolve.resolve_imports parsed_ast in
    if verbose then print_endline (Pprint_past.string_of_program resolved_ast);

    print_endline "[*] Typechecking...";
    let typed_ast = Hm.annotate_program resolved_ast in
    if verbose then print_endline (Pprint_tast.string_of_program typed_ast);

    print_endline "[*] Desugaring...";
    let desugared_ast = Desugar.desugar_program typed_ast in
    print_endline (Pprint_dast.string_of_program desugared_ast);

    print_endline "[*] Lowering to MIR...";
    (* let alpha_renamed_ast = Alpha.Alpha.rename desugared_ast in *)
    (* print_endline (Pprint_dast.string_of_program alpha_renamed_ast); *)
    let alpha_prog = Alpha2.alpha_program desugared_ast in
    print_endline (Pprint_dast.string_of_program alpha_prog);

    print_endline "[*] Lowering to ANF...";
    (* let anf = Anf.Anf.convert alpha_renamed_ast in *)
    (* print_endline (Anf.Pprint_anf.string_of_program anf); *)
    (* let anf2 = Anf2.anf_program desugared_ast in *)
    (* print_endline (Pprint_dast.string_of_program anf2); *)
    let _anf3 = Anf3.anf_of_program desugared_ast in
    print_endline (Anf3.string_of_anf_program desugared_ast);
    (* let lowered_ast = Lower.lower_program alpha_renamed_ast in *)
    (* if verbose then print_endline (Pprint_mir.string_of_program lowered_ast); *)

    (* print_endline "[*] Generating LLVM IR..."; *)
    (* Llvm_ir_gen.Codegen.codegen_program lowered_ast; *)
    (* Llvm_ir_gen.Codegen.save_module_to_file "llvm_bin/output.ll"; *)
    (* if true then Llvm_ir_gen.Codegen.print_module_to_stderr (); *)

    print_endline "[*] Done!"
  with
  | Lexer.SyntaxError msg ->
      Printf.eprintf "[!] Lexer error at %s: %s\n" (print_position lexbuf) msg
  | Parser.Error ->
      Printf.eprintf "[!] Parser error at %s\n" (print_position lexbuf)
  | Resolve.ImportError msg -> Printf.eprintf "[!] Import error: %s\n" msg
  | Hm.TypeError msg -> Printf.eprintf "[!] Type error at: %s\n" msg
  (* | Llvm_ir_gen.Codegen.LLVMError msg -> Printf.eprintf "LLVM error: %s\n" msg *)
