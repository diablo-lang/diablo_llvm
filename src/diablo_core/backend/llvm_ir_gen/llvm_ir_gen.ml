open Llvm
open Ast.Ast_types
open Typing

module Codegen = struct
  exception LLVMError of string

  let context = global_context ()
  let diablo_module = create_module context "DiabloModule"
  let builder = builder context
  let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 10
  let i32_t = i32_type context
  let i1_t = i1_type context

  let i8_t = i8_type context

  let void_t = void_type context

  let llvm_type = function
    | Type.TInt -> i32_t
    | Type.TBool -> i1_t
    | Type.TVoid -> void_type context
    | Type.TString -> pointer_type context
    | _ -> raise (LLVMError "Unsupported type")

  let declare_extern_function name return_type param_types diablo_module =
    declare_function name
      (function_type (llvm_type return_type)
         (Array.of_list (List.map llvm_type param_types)))
      diablo_module

  let rec codegen_expr = function
    | Typed_ast.Integer n -> const_int i32_t n
    | Typed_ast.Boolean b -> const_int i1_t (if b then 1 else 0)
    | Typed_ast.StringLiteral s ->
        build_global_stringptr s "str" builder
    | Typed_ast.Identifier name -> (
        try Hashtbl.find named_values name
        with Not_found -> raise (LLVMError ("Unknown variable: " ^ name)))
    | Typed_ast.BinOp (_, op, lhs, rhs) -> (
        let lhs_val = codegen_expr lhs in
        let rhs_val = codegen_expr rhs in
        match op with
        | BinOpPlus -> build_add lhs_val rhs_val "addtmp" builder
        | BinOpMinus -> build_sub lhs_val rhs_val "subtmp" builder
        | BinOpMult -> build_mul lhs_val rhs_val "multmp" builder
        | BinOpDiv -> build_sdiv lhs_val rhs_val "divtmp" builder
        | BinOpRem -> build_srem lhs_val rhs_val "modtmp" builder
        | BinOpLessThan -> build_icmp Icmp.Slt lhs_val rhs_val "lttmp" builder
        | BinOpGreaterThan ->
            build_icmp Icmp.Sgt lhs_val rhs_val "gttmp" builder
        | BinOpLessThanEqual ->
            build_icmp Icmp.Sle lhs_val rhs_val "letmp" builder
        | BinOpGreaterThanEqual ->
            build_icmp Icmp.Sge lhs_val rhs_val "getmp" builder
        | BinOpAnd -> build_and lhs_val rhs_val "andtmp" builder
        | BinOpOr -> build_or lhs_val rhs_val "ortmp" builder
        | BinOpEqual -> build_icmp Icmp.Eq lhs_val rhs_val "eqtmp" builder
        | BinOpNotEqual -> build_icmp Icmp.Ne lhs_val rhs_val "netmp" builder)
    | Typed_ast.UnOp (_, op, expr) -> (
        let expr_val = codegen_expr expr in
        match op with
        | UnOpNegate -> build_neg expr_val "negtmp" builder
        | UnOpNot -> build_not expr_val "nottmp" builder)
    | Typed_ast.Call (ret_type, callee, args) ->
        let callee =
          match lookup_function callee diablo_module with
          | Some callee -> callee
          | None -> raise (LLVMError ("Unknown function referenced: " ^ callee))
        in
        let params = params callee in
        if Array.length params == List.length args then ()
        else
          raise
            (LLVMError
               ("Expected "
               ^ string_of_int (Array.length params)
               ^ " arguments, got "
               ^ string_of_int (List.length args)));
        let arr_args = Array.of_list (List.map codegen_expr args) in
        (* TODO: Set the function type to the type of the callee *)
        (* ?: Maybe this should use previously inferred types *)

        let ret_type = llvm_type ret_type in
        let arg_types = Array.of_list (List.map (fun arg -> 
          match arg with 
          | Typed_ast.Integer _ -> llvm_type Type.TInt
          | Typed_ast.Boolean _ -> llvm_type Type.TBool
          | Typed_ast.StringLiteral _ -> llvm_type Type.TString
          | _ -> raise (LLVMError "Unsupported type")
        ) args) in
        let fnty = function_type ret_type arg_types in

        (* let ft = function_type (return_type (type_of callee)) (param_types (type_of callee)) in *)
        build_call fnty callee arr_args "calltmp" builder
    | Typed_ast.Let (_, name, expr) ->
        let expr_val = codegen_expr expr in
        Hashtbl.add named_values name expr_val;
        expr_val
    | Typed_ast.ExternCall (_, name, args) ->
        let callee =
          match lookup_function name diablo_module with
          | Some callee -> callee
          | None -> raise (LLVMError ("Unknown function referenced: " ^ name))
        in
        let args = Array.map codegen_expr (Array.of_list args) in
        let ft =
          function_type (llvm_type Type.TInt)
            (Array.make (Array.length args) (llvm_type Type.TInt))
        in
        build_call ft callee args "calltmp" builder
    | _ -> raise (LLVMError "Unknown expression")

  (* ?: Look at note about append_block below *)
  let codegen_block = function
    (* If the block is empty, return a void value for the function's return type. *)
    | Typed_ast.Block (_, []) ->
        build_ret_void builder
    (* If the block is not empty, return the last expression. *)
    | Typed_ast.Block (_, exprs) ->
        List.fold_left
          (fun _ expr -> codegen_expr expr)
          (const_null (i32_type context))
          exprs

  let codegen_function = function
    | Typed_ast.TFunction (name, params, return_type, body) -> (
        Hashtbl.clear named_values;

        let param_types =
          List.map (fun (TParam (t, _)) -> llvm_type t) params
        in
        let func_type =
          function_type (llvm_type return_type) (Array.of_list param_types)
        in

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
          raise (LLVMError (Printexc.to_string e)))

  (* Handling the special case for the main function *)
  let codegen_main (body : Typed_ast.block) =
    (* Create a new function type for main: i32 @main() *)
    let main_type =
      function_type (llvm_type Type.TInt) (Array.make 0 (llvm_type Type.TInt))
    in
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

  let declare_extern_functions () =
    Hashtbl.add named_values "ext_add" (declare_extern_function "ext_add" Type.TInt [ Type.TInt; Type.TInt ] diablo_module);
    let printf_ty = var_arg_function_type i32_t [| pointer_type (type_context i8_t) |] in
    let printf = declare_function "printf" printf_ty diablo_module in
    Hashtbl.add named_values "printf" printf

  let codegen_program = function
    | Typed_ast.Program (_import_stms, funcs, main_block) ->
        Printf.printf "Compiling %d functions\n" (List.length funcs);
        declare_extern_functions ();

        List.iter (fun func -> ignore (codegen_function func)) funcs;
        ignore (codegen_main main_block)

  let print_module_to_stderr () =
    dump_module diablo_module

  let save_module_to_file filename =
    print_module filename diablo_module
end
