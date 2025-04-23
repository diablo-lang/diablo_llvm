open Llvm
open Ast.Ast_types
open Lowering

module Codegen = struct
  exception LLVMError of string

  let context = global_context ()
  let diablo_module = create_module context "DiabloModule"
  let builder = builder context
  let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 10
  let i32_t = i32_type context
  let bool_t = i1_type context
  let i8_t = i8_type context
  let void_t = void_type context

  let llvm_type = function
    | TConst "int" -> i32_t
    | TConst "bool" -> bool_t
    | TConst "str" -> pointer_type context
    | TConst "unit" -> void_type context
    | TArrow _ -> i32_t
    | ty -> raise (LLVMError ("Unsupported type" ^ string_of_ty ty))

  let rec codegen_expr = function
    | Lowered_ast.Integer n -> const_int i32_t n
    | Lowered_ast.Boolean b -> const_int bool_t (if b then 1 else 0)
    | Lowered_ast.Unit -> const_null void_t
    | Lowered_ast.StringLiteral s -> build_global_stringptr s "strtmp" builder
    (* TODO: Review *)
    | Lowered_ast.Identifier (name, ty) -> (
        try match ty with
        | TConst "int" | TConst "bool" | TConst "str" | TConst "unit" ->
            print_endline ("load" ^ name ^ string_of_ty ty ^ "\n");
            build_load (llvm_type ty)
              (Hashtbl.find named_values name)
              name builder
        | TArrow _ ->
            Printf.printf "funcccc %s\n" name;
            Hashtbl.find named_values name
        | _ ->
            raise (LLVMError ("Unsupported type" ^ string_of_ty ty));
        with Not_found -> raise (LLVMError ("Unknown variable " ^ name)))
    | Lowered_ast.BinOp (op, lhs, rhs, _ty) -> (
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
    | Lowered_ast.UnOp (op, expr, _ty) -> (
        let expr_val = codegen_expr expr in
        match op with
        | UnOpNegate -> build_neg expr_val "negtmp" builder
        | UnOpNot -> build_not expr_val "nottmp" builder)
    (* TODO: Review *)
    | Lowered_ast.If (cond, then_expr, else_expr, _ty) ->
        let cond_val = codegen_expr cond in
        let then_val = codegen_expr then_expr in
        let else_val = codegen_expr else_expr in
        build_cond_br cond_val (block_of_value then_val) (block_of_value else_val) builder
    | Lowered_ast.List (_exprs, _ty) ->
      raise (LLVMError "[List] Unsupported type")
    (* TODO: Review *)
    | Lowered_ast.Call (callee, args, ret_type) ->
      let callee = codegen_expr callee in
      let arg_tys =
        Array.of_list
          (List.map
             (fun arg ->
               match arg with
               | Lowered_ast.Integer _ -> llvm_type (TConst "int")
               | Lowered_ast.Boolean _ -> llvm_type (TConst "bool")
               | Lowered_ast.StringLiteral _ -> llvm_type (TConst "str")
               | _ -> raise (LLVMError "[Call] Unsupported type for argument"))
             args)
      in
      let args = List.map codegen_expr args |> Array.of_list in
      let fnty = function_type (llvm_type ret_type) arg_tys in
      build_call fnty callee args "calltmp" builder
    (* TODO: Review *)
    | Lowered_ast.LetIn (name, expr, body, ty) ->
      let expr_value = codegen_expr expr in
      let alloca = build_alloca (llvm_type ty) name builder in
      ignore (build_store expr_value alloca builder);
      codegen_expr body
    (* TODO: Review *)
    | Lowered_ast.Lambda (params, body, return_type) ->
        let param_types = List.map (fun (_, ty) -> llvm_type ty) params in
        let func_type =
          function_type (llvm_type return_type) (Array.of_list param_types)
        in
        let fn = declare_function "lambda" func_type diablo_module in
        Hashtbl.add named_values "lambda" fn;
        Printf.printf "In Lambda: %s\n" (string_of_llvalue fn); flush stdout;
        let _builder = builder_at_end context (entry_block fn) in
        let _params =
          List.map
            (fun (name, _ty) ->
              let param = param fn (List.length params) in
              set_value_name name param;
              Hashtbl.add named_values name param;
              param)
            params
        in
        Printf.printf "Past Lambda: %s\n" (string_of_llvalue fn); flush stdout;
        let body_val = codegen_expr body in
        Printf.printf "Pasts Lambda: %s\n" (string_of_llvalue fn); flush stdout;
        Hashtbl.remove named_values "lambda";
        Printf.printf "Done Lambda: %s\n" (string_of_llvalue fn); flush stdout;
        body_val

  let declare_extern_function name return_type param_types diablo_module =
    declare_function name
      (function_type (llvm_type return_type)
          (Array.of_list (List.map llvm_type param_types)))
      diablo_module

  let declare_extern_functions () =
    Hashtbl.add named_values "ext_add"
      (declare_extern_function "ext_add" (TConst "int")
         [ TConst "int"; TConst "int" ]
         diablo_module);
    let printf_ty =
      var_arg_function_type i32_t [| pointer_type (type_context i8_t) |]
    in
    let printf = declare_function "printf" printf_ty diablo_module in
    Hashtbl.add named_values "printf" printf

  let codegen_top_level_declaration = function
    | Lowered_ast.Let(name, value_expr, _ty) ->
        let expr_val = codegen_expr value_expr in
        Hashtbl.add named_values name expr_val

  let codegen_program = function
    | Lowered_ast.Program (decls, _program_ty) ->
        Printf.printf "Compiling %d declarations\n" (List.length decls);
        declare_extern_functions ();
        List.iter codegen_top_level_declaration decls

  let print_module_to_stderr () = dump_module diablo_module
  let save_module_to_file filename = print_module filename diablo_module
end
