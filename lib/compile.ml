open Parsing
open Typing
open Lexing

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  Printf.sprintf "line %d, column %d" line col

let compile (s : string) =
  let lexbuf = Lexing.from_string s in
  print_endline "[*] Parsing...";
  try
    let program = Parser.program Lexer.read_token lexbuf in
    print_endline (Pprint_past.pprint_program program);
    print_endline "[*] Typechecking...";
    let program_type = Hm.type_check program in
    print_endline (Pprint_tast.pprint_type program_type);
    print_endline "[*] Desugaring...";
    print_endline "[*] Lowering to LLVM IR...";
    print_endline "[*] Done!";
  with
  | Lexer.SyntaxError msg -> Printf.eprintf "Lexer error: %s\n" msg
  | Parser.Error -> Printf.eprintf "Parser error at %s\n" (print_position lexbuf) 
  | Hm.TypeError msg -> Printf.eprintf "Type error: %s\n" msg
