open Lexing
open Parsing

let () =
  let lexbuf = from_channel stdin in
  try
    let _ = Parser.program Lexer.read_token lexbuf in
    print_endline "Parsed successfully!"
  with
  | Lexer.SyntaxError msg -> Printf.eprintf "Syntax error: %s\n" msg
  | _ -> Printf.eprintf "Unknown error\n"