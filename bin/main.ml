open Diablo.Compile

let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Error: Please provide a filename as the first argument.\n"
  else
    let filename = Sys.argv.(1) in
    let source = read_file filename in
    let verbose = false in
    compile source verbose
