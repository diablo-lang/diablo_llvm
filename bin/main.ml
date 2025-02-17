open Parsing
open Diablo.Compile
open Typing

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
      compile s
  with
  | Lexer.SyntaxError msg -> Printf.printf "Lexer error: %s\n" msg
  | Parser.Error -> Printf.printf "Parser error\n"
  | Hm.TypeError msg -> Printf.printf "Type error: %s\n" msg
  | Hm.Unify.UnificationError msg -> Printf.printf "Unification error: %s\n" msg
  | _ -> Printf.printf "Unknown error\n"