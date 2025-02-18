open Llvm
open Ast.Ast_types
open Parsing

module Codegen = struct
  exception LLVMError of string
  let context = global_context ()
  let diablo_module = create_module context "diablo_module"
  let builder = builder context
  let named_values: (string, llvalue) Hashtbl.t = Hashtbl.create 10

  let int_type = i32_type context
  let bool_type = i1_type context

  let llvm_type = function
    | Type.TInt -> int_type
    | Type.TBool -> bool_type
    | _ -> raise (LLVMError "Unsupported type")


  let rec codegen_expr = function
  | Parsed_ast.Integer n -> const_int int_type n
  | Parsed_ast.Boolean b -> const_int bool_type (if b then 1 else 0)
  | Parsed_ast.Identifier name ->
    (try Hashtbl.find named_values name with
    | Not_found -> raise (LLVMError ("Unknown variable: " ^ name)))
  | Parsed_ast.BinOp (op, lhs, rhs) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    begin
      match op with
      | BinOpPlus -> build_add lhs_val rhs_val "addtmp" builder
      | BinOpMinus -> build_sub lhs_val rhs_val "subtmp" builder
      | BinOpMult -> build_mul lhs_val rhs_val "multmp" builder
      | BinOpDiv -> build_sdiv lhs_val rhs_val "divtmp" builder
      | BinOpRem -> build_srem lhs_val rhs_val "modtmp" builder
      | BinOpLessThan -> build_icmp Icmp.Slt lhs_val rhs_val "lttmp" builder
      | BinOpGreaterThan -> build_icmp Icmp.Sgt lhs_val rhs_val "gttmp" builder
      | BinOpLessThanEqual -> build_icmp Icmp.Sle lhs_val rhs_val "letmp" builder
      | BinOpGreaterThanEqual -> build_icmp Icmp.Sge lhs_val rhs_val "getmp" builder
      | BinOpAnd -> build_and lhs_val rhs_val "andtmp" builder
      | BinOpOr -> build_or lhs_val rhs_val "ortmp" builder
      | BinOpEqual -> build_icmp Icmp.Eq lhs_val rhs_val "eqtmp" builder
      | BinOpNotEqual -> build_icmp Icmp.Ne lhs_val rhs_val "netmp" builder
    end
  | Parsed_ast.UnOp (op, expr) ->
    let expr_val = codegen_expr expr in
    begin
      match op with
      | UnOpNegate -> build_neg expr_val "negtmp" builder
      | UnOpNot -> build_not expr_val "nottmp" builder
    end
  | Parsed_ast.Call (callee, args) ->
    let callee =
      match lookup_function callee diablo_module with
      | Some callee -> callee
      | None -> raise (LLVMError ("Unknown function referenced: " ^ callee))
    in
    let params = params callee in
    if Array.length params == List.length args then () else
      raise (LLVMError ("Expected " ^ (string_of_int (Array.length params)) ^ " arguments, got " ^ (string_of_int (List.length args))));
    let args = Array.map codegen_expr (Array.of_list args) in
    (* TODO: Set the function type to the type of the callee *)
    (* ?: Maybe this should use previously inferred types *)
    (* let ft = function_type (return_type (type_of callee)) (param_types (type_of callee)) in *)
    let ft = function_type(llvm_type Type.TInt) (Array.make (Array.length args) (llvm_type Type.TInt)) in
    build_call ft callee args "calltmp" builder
  | Parsed_ast.Let (name, expr) ->
    let expr_val = codegen_expr expr in
    Hashtbl.add named_values name expr_val;
    expr_val
  | _ -> raise (LLVMError "Unknown expression")

  (* ?: Look at note about append_block below *)
  let codegen_block = function
  | Parsed_ast.Block [] ->
    (* If the block is empty, return a null value for the function's return type. *)
    build_ret (const_null (llvm_type Type.TVoid)) builder
    (* If the block is not empty, return the last expression. *)
  | Parsed_ast.Block exprs ->
    List.fold_left (fun _ expr -> codegen_expr expr) (const_null (i32_type context)) exprs

  let codegen_function = function
  | Parsed_ast.TFunction (name, params, return_type, body) ->
    Hashtbl.clear named_values;

    let param_types = List.map (fun (TParam (t, _)) -> llvm_type t) params in
    let func_type = function_type (llvm_type return_type) (Array.of_list param_types) in

    (* ?: Maybe use declare_function instead and check if the function already exists *)
    let the_function = 
      match lookup_function name diablo_module with
      | None -> declare_function name func_type diablo_module
      | Some f ->
        if block_begin f <> At_end f then
          raise (LLVMError "Function already defined");
        if element_type (type_of f) <> func_type then
          raise (LLVMError "Function already defined with different type");
        f
    in

    List.iteri
      (fun i (TParam (_, pname)) ->
        let param = param the_function i in
        set_value_name pname param;
        Hashtbl.add named_values pname param)
      params;

    (* ?: Should this be moved into codegen_block *)
    (* The problem is that "the_function" used in the line below is specific to the current function *)
    let bb = append_block context "entry" the_function in
    position_at_end bb builder;

    try
      let ret_val = codegen_block body in
      let _ = build_ret ret_val builder in
      Llvm_analysis.assert_valid_function the_function;
      the_function
    with e ->
      delete_function the_function;
      raise (LLVMError (Printexc.to_string e))


  (* Handling the special case for the main function *)
  let codegen_main (body: Parsed_ast.block) =
    (* Create a new function type for main: i32 @main() *)
    let main_type = function_type (llvm_type Type.TInt) (Array.make 0 (llvm_type Type.TInt)) in
    let main_func = declare_function "main" main_type diablo_module in

    (* Create a new entry block for main *)
    let bb = append_block context "entry" main_func in
    position_at_end bb builder;

    try
      (* Generate the body of the main function *)
      let ret_val = codegen_block body in

      (* Return the computed value in main *)
      let _ = build_ret ret_val builder in

      (* Validate the generated main function *)
      Llvm_analysis.assert_valid_function main_func;

      main_func
    with e ->
      (* If there is an error, delete the main function *)
      delete_function main_func;
      raise (LLVMError (Printexc.to_string e))

    
  let rec string_of_block = function
    | Parsed_ast.Block exprs ->
      List.fold_left (fun acc expr -> acc ^ (string_of_expr expr) ^ "\n") "" exprs
    
    and string_of_expr = function
    | Parsed_ast.Integer n -> string_of_int n
    | Parsed_ast.Boolean b -> string_of_bool b
    | Parsed_ast.Identifier s -> s
    | Parsed_ast.BinOp (_op, lhs, rhs) ->
      Printf.sprintf "(%s %s)" (string_of_expr lhs) (string_of_expr rhs)
    | Parsed_ast.UnOp (_op, expr) ->
      Printf.sprintf "(%s)" (string_of_expr expr)
    | Parsed_ast.Let (name, expr) ->
      Printf.sprintf "let %s = %s" name (string_of_expr expr)
    | Parsed_ast.If (cond, then_branch, else_branch) ->
      Printf.sprintf "if %s then %s else %s" (string_of_expr cond) (string_of_block then_branch) (string_of_block else_branch)
    | Parsed_ast.Call (callee, args) ->
      Printf.sprintf "%s(%s)" callee (String.concat ", " (List.map string_of_expr args))
    

  let codegen_program = function
  | Parsed_ast.Program (funcs, main_block) ->
    List.iter (fun func -> ignore (codegen_function func)) funcs;
    ignore (codegen_main main_block)
 
  let dump_ir () = dump_module diablo_module

  let save_ir_to_file filename =
    let oc = open_out filename in
    output_string oc (Llvm.string_of_llmodule diablo_module);
    close_out oc
end
