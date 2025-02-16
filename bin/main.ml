
open Parsing

let parse (s : string): Parsed_ast.program =
  let lexbuf = Lexing.from_string s in
  let program = Parser.program Lexer.read_token lexbuf in
  program

let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content
  
let () =
  try
    (* Check if a filename is provided as the first argument *)
    if Array.length Sys.argv < 2 then
      (* No filename provided, so print an error and exit *)
      Printf.printf "Error: Please provide a filename as the first argument.\n"
    else
      let filename = Sys.argv.(1) in  (* Get the filename from the first argument *)
      let s = read_file filename in   (* Read the entire file contents *)
      print_endline s;
      let program = parse s in        (* Parse the content *)
      print_endline (Pprint_past.pprint_program program)
  with
  | Lexer.SyntaxError msg -> Printf.printf "Lexer error: %s\n" msg
  | Parser.Error -> Printf.printf "Parser error\n"
  | _ -> Printf.printf "Unknown error\n"