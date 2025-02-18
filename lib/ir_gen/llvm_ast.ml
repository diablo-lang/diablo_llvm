open Llvm
open Ast.Ast_types
open Typing.Typed_ast
open Typing.Pprint_tast

let print_expr expr = print_endline (pprint_expr expr)

let print_exprs
    (exprs : expr list) =
  List.iter print_expr exprs

module Codegen = struct
  let context = global_context ()
  let the_module = create_module context "diablo"
  let builder = builder context

  (* Convert AST Types to LLVM Types *)
  let rec llvm_type = function
    | Type.TInt -> i32_type context
    | Type.TBool -> i1_type context
    | Type.TVoid -> void_type context
    | Type.TVar _ -> failwith "Type variables should be resolved before codegen"
    | Type.TFun (args, ret) ->
        let llvm_args = Array.of_list (List.map llvm_type args) in
        function_type (llvm_type ret) llvm_args

  (* Generate LLVM Code for Binary Operators *)
  let codegen_binop op lhs rhs =
    match op with
    | BinOpPlus -> build_add lhs rhs "addtmp" builder
    | BinOpMinus -> build_sub lhs rhs "subtmp" builder
    | BinOpMult -> build_mul lhs rhs "multmp" builder
    | BinOpDiv -> build_sdiv lhs rhs "divtmp" builder
    | BinOpRem -> build_srem lhs rhs "modtmp" builder
    | BinOpLessThan -> build_icmp Icmp.Slt lhs rhs "lttmp" builder
    | BinOpGreaterThan -> build_icmp Icmp.Sgt lhs rhs "gttmp" builder
    | BinOpLessThanEqual -> build_icmp Icmp.Sle lhs rhs "letmp" builder
    | BinOpGreaterThanEqual -> build_icmp Icmp.Sge lhs rhs "getmp" builder
    | BinOpAnd -> build_and lhs rhs "andtmp" builder
    | BinOpOr -> build_or lhs rhs "ortmp" builder
    | BinOpEqual -> build_icmp Icmp.Eq lhs rhs "eqtmp" builder
    | BinOpNotEqual -> build_icmp Icmp.Ne lhs rhs "netmp" builder

  (* Generate LLVM Code for Unary Operators *)
  let codegen_unop op v =
    match op with
    | UnOpNot -> build_not v "nottmp" builder
    | UnOpNegate -> build_neg v "negtmp" builder

  (* Codegen for Expressions *)
  let rec codegen_expr env = function
    | Identifier name ->
        (match Hashtbl.find_opt env name with
        | Some v -> v
        | None -> failwith ("Unknown variable: " ^ name))
    | Integer i -> const_int (i32_type context) i
    | Boolean b -> const_int (i1_type context) (if b then 1 else 0)
    | UnOp (_, op, expr) -> codegen_unop op (codegen_expr env expr)
    | BinOp (_, op, lhs, rhs) ->
        let lval = codegen_expr env lhs in
        let rval = codegen_expr env rhs in
        codegen_binop op lval rval
    | Let (_, name, value) ->
        let v = codegen_expr env value in
        Hashtbl.add env name v;
        v
    | If (_, cond, then_block, else_block) ->
        let cond_val = codegen_expr env cond in
        let zero = const_int (i1_type context) 0 in
        let cond_bool = build_icmp Icmp.Ne cond_val zero "ifcond" builder in
        let start_bb = insertion_block builder in
        let func = block_parent start_bb in
        let then_bb = append_block context "then" func in
        let else_bb = append_block context "else" func in
        let merge_bb = append_block context "ifcont" func in
        ignore (build_cond_br cond_bool then_bb else_bb builder);
        
        (* Then block *)
        position_at_end then_bb builder;
        let then_val = codegen_block env then_block in
        ignore (build_br merge_bb builder);
        
        (* Else block *)
        position_at_end else_bb builder;
        let else_val = codegen_block env else_block in
        ignore (build_br merge_bb builder);

        (* Merge block *)
        position_at_end merge_bb builder;
        build_phi [(then_val, then_bb); (else_val, else_bb)] "iftmp" builder
    | Call (ast_type, fname, args) ->
        let func_type = llvm_type ast_type in
        let func = lookup_function fname the_module in
        (match func with
        | Some f ->
            let args = Array.of_list (List.map (codegen_expr env) args) in
            build_call func_type f args "calltmp" builder
        | None -> failwith ("Unknown function: " ^ fname))

  (* Codegen for Blocks *)
  and codegen_block env (Block (_, exprs)) =
    print_endline "--- Generating code for block";
    print_exprs exprs;
    let last_value = List.fold_left (fun _ expr -> codegen_expr env expr) (const_int (i32_type context) 0) exprs in
    (* Make sure the last value is returned or terminates the block *)
    Printf.printf "Last value: %s\n" (value_name last_value);
    last_value

  (* Codegen for Functions *)
  let codegen_function (TFunction (name, params, ret_type, body)) =
    print_endline ("--- Generating code for function: " ^ name);
    let llvm_ret_type = llvm_type ret_type in
    let llvm_param_types = Array.of_list (List.map (fun (TParam (t, _)) -> llvm_type t) params) in
    let func_type = function_type llvm_ret_type llvm_param_types in
    let func = define_function name func_type the_module in
    let entry_bb = append_block context "entry" func in
    position_at_end entry_bb builder;
    let env = Hashtbl.create 1000 in
    List.iteri
      (fun i (TParam (_, pname)) ->
        let param = param func i in
        set_value_name pname param;
        Hashtbl.add env pname param)
      params;

    let last_value = codegen_block env body in
    (* If no value was generated, return a default value *)
    if last_value = const_int (i32_type context) 0 then
      ignore (build_ret (const_int llvm_ret_type 0) builder)
    else
      ignore (build_ret last_value builder);

    Llvm_analysis.assert_valid_function func;
    func

  (* Codegen for Program *)
  let codegen_program (Program (funcs, main_block)) =
    List.iter (fun func -> ignore (codegen_function func)) funcs;
    ignore (codegen_block (Hashtbl.create 1000) main_block)

  (* Output LLVM IR *)
  let dump_ir () = dump_module the_module

  let save_ir_to_file filename =
    let oc = open_out filename in
    output_string oc (Llvm.string_of_llmodule the_module);
    close_out oc
end