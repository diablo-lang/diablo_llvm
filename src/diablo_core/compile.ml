open Parsing
open Typing
open Lexing
open Ir_gen
open Resolution

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
    (* let program_type = Hm.type_check resolved_ast in *)
    (* let typed_ast = Hm.convert_to_typed_ast resolved_ast in *)
    (* print_endline (Pprint_tast.pprint_program typed_ast); *)
    (* print_endline (Pprint_tast.pprint_type program_type); *)

    print_endline "[*] Desugaring...";

    print_endline "[*] Lowering to LLVM IR...";
    Llvm_ir_gen.Codegen.codegen_program resolved_ast;
    Llvm_ir_gen.Codegen.dump_ir ();
    Llvm_ir_gen.Codegen.save_ir_to_file "llvm_bin/output.ll";

    print_endline "[*] Done!";
  with
  | Lexer.SyntaxError msg -> Printf.eprintf "Lexer error at %s: %s\n" (print_position lexbuf) msg
  | Parser.Error -> Printf.eprintf "Parser error at %s\n" (print_position lexbuf) 
  | Hm.TypeError msg -> Printf.eprintf "Type error at: %s\n" msg
  | Llvm_ir_gen.Codegen.LLVMError msg -> Printf.eprintf "LLVM error: %s\n" msg
