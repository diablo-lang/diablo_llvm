open Parsing

exception ImportError of string

let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let load_module (module_name : string) : Parsed_ast.function_defn list =
  (* Stub function: Replace with actual file reading/parsing logic *)
  match module_name with
  | "math" ->
    let content = read_file "src/stdlib/math/math.diablo" in
    let lexbuf = Lexing.from_string content in
    let parsed_ast = Parser.program Lexer.read_token lexbuf in
    let funcs = match parsed_ast with
      | Parsed_ast.Program (_, funcs, _) -> funcs
    in
    funcs
  | "net" ->
    let content = read_file "src/stdlib/net/socket.diablo" in
    let lexbuf = Lexing.from_string content in
    let parsed_ast = Parser.program Lexer.read_token lexbuf in
    let funcs = match parsed_ast with
      | Parsed_ast.Program (_, funcs, _) -> funcs
    in
    funcs
  | "socket" ->
    let content = read_file "src/stdlib/net/http.diablo" in
    let lexbuf = Lexing.from_string content in
    let parsed_ast = Parser.program Lexer.read_token lexbuf in
    let funcs = match parsed_ast with
      | Parsed_ast.Program (_, funcs, _) -> funcs
    in
    funcs
  | _ -> raise (ImportError ("Module not found: " ^ module_name))

(* let load_module (module_name : string) : Parsed_ast.function_defn list =
  (* Stub function: Replace with actual file reading/parsing logic *)
  match module_name with
  | "math" -> [Parsed_ast.TFunction ("add", [TParam (Type.TInt, "x"); TParam (Type.TInt, "y")], Type.TInt, Block [
    BinOp (BinOpPlus, Identifier "x", Identifier "y")
  ])]
  | _ -> raise (ImportError ("Module not found: " ^ module_name)) *)

let resolve_imports (program : Parsed_ast.program) : Parsed_ast.program =
  let Parsed_ast.Program (imports, funcs, main) = program in
  let imported_funcs =
    List.fold_left
      (fun acc (Parsed_ast.Import module_name) -> acc @ load_module module_name)
      []
      imports
  in
  Program ([], funcs @ imported_funcs, main)  (* Remove imports after resolution *)