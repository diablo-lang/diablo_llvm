
open Diablo
let parse (s : string): Ast.expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.program Lexer.read_token lexbuf in
  ast

let () =
  try
    let s = read_line () in
    let ast = parse s in
    print_endline (Ast.pprint_ast ast)
  with
  | Lexer.SyntaxError msg -> Printf.printf "Lexer error: %s\n" msg
  | Parser.Error -> Printf.printf "Parser error\n"
  | _ -> Printf.printf "Unknown error\n"