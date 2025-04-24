open Parsing

exception ImportError of string

let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let load_module_content module_name =
  let filename =
    match module_name with
    | "math" -> "src/stdlib/math/math.diablo"
    | "net" -> "src/stdlib/net/socket.diablo"
    | "socket" -> "src/stdlib/net/http.diablo"
    | _ -> raise (ImportError ("Module not found: " ^ module_name))
  in
  let content = read_file filename in
  let lexbuf = Lexing.from_string content in
  let parsed_module_ast = Parser.module_file Lexer.read_token lexbuf in
  match parsed_module_ast with
  | Parsed_ast.Module (imports, module_defn) -> (imports, module_defn)

let rec load_module (module_name : string) (loaded_modules : string list) :
    Parsed_ast.top_level_declaration list =
  if List.mem module_name loaded_modules then []
  else
    let imports, module_defn = load_module_content module_name in
    let new_loaded_modules = module_name :: loaded_modules in
    let recursive_declarations =
      List.fold_left
        (fun acc (Parsed_ast.Import imported_module) ->
          acc @ load_module imported_module new_loaded_modules)
        [] imports
    in
    match module_defn with
    | Parsed_ast.ModuleDefinition (_, decls) -> decls @ recursive_declarations

let resolve_imports (program : Parsed_ast.program) : Parsed_ast.program =
  let (Parsed_ast.Program (import_stmts, top_level_decls)) = program in
  let imported_decls =
    List.fold_left
      (fun acc (Parsed_ast.Import module_name) ->
        acc @ load_module module_name [])
      [] import_stmts
  in
  Program ([], top_level_decls @ imported_decls)
