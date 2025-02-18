(* open Llvm
open Typing
open Ast.Ast_types

module Codegen = struct
  (* Create an LLVM context and module *)
  let context = global_context ()
  let the_module = create_module context "my_compiler"
  let builder = builder context
  let named_values = Hashtbl.create 10 (* Store variables *)
  
  module StringMap = Map.Make(String)
  type llvm_env = llvalue StringMap.t

  (* Type mapping from our AST types to LLVM types *)
  let llvm_type = function
    | Type.TInt -> i32_type context
    | Type.TBool -> i1_type context
    | Type.TVoid -> void_type context
    | _ -> failwith "Unsupported type"

  (* Generate code for an expression *)
  let rec codegen_expr env = function
    | Typed_ast.Integer n ->
        const_int (i32_type context) n

    | Typed_ast.Boolean b ->
        const_int (i1_type context) (if b then 1 else 0)

    | Typed_ast.Identifier name -> StringMap.find name env

    | Typed_ast.BinOp (_, op, lhs, rhs) ->
        let lval = codegen_expr env lhs in
        let rval = codegen_expr env rhs in
        (match op with
        | BinOpPlus  -> build_add lval rval "addtmp" builder
        | BinOpMinus -> build_sub lval rval "subtmp" builder
        | BinOpMult  -> build_mul lval rval "multmp" builder
        | BinOpDiv   -> build_sdiv lval rval "divtmp" builder
        | BinOpRem   -> build_srem lval rval "remtmp" builder
        | BinOpLessThan -> build_icmp Icmp.Slt lval rval "lttmp" builder
        | BinOpGreaterThan -> build_icmp Icmp.Sgt lval rval "gttmp" builder
        | BinOpLessThanEqual -> build_icmp Icmp.Sle lval rval "ltepmp" builder
        | BinOpGreaterThanEqual -> build_icmp Icmp.Sge lval rval "gtepmp" builder
        | BinOpEqual -> build_icmp Icmp.Eq lval rval "eqtmp" builder
        | BinOpNotEqual -> build_icmp Icmp.Ne lval rval "netmp" builder
        | BinOpAnd -> build_and lval rval "andtmp" builder
        | BinOpOr -> build_or lval rval "ortmp" builder)

    | Typed_ast.UnOp (_, op, e) ->
        let val_ = codegen_expr env e in
        (match op with
         | UnOpNegate -> build_neg val_ "negtmp" builder
         | UnOpNot -> build_not val_ "nottmp" builder)

    | Typed_ast.Let (_, name, value) ->
        let init_val = codegen_expr env value in
        let alloca = build_alloca (type_of init_val) name builder in
        ignore (build_store init_val alloca builder);
        Hashtbl.add named_values name alloca;
        init_val

    | Typed_ast.If (_, cond, then_block, else_block) ->
        let cond_val = codegen_expr env cond in
        let zero = const_int (i1_type context) 0 in
        let _cond_bool = build_icmp Icmp.Ne cond_val zero "ifcond" builder in

        (* Create blocks *)
        let the_function = block_parent (insertion_block builder) in
        let then_bb = append_block context "then" the_function in
        let else_bb = append_block context "else" the_function in
        let merge_bb = append_block context "ifcont" the_function in

        (* Generate code for then block *)
        position_at_end then_bb builder;
        let then_val = codegen_block then_block env in
        ignore (build_br merge_bb builder);

        (* Generate code for else block *)
        position_at_end else_bb builder;
        let else_val = codegen_block else_block env in
        ignore (build_br merge_bb builder);

        (* Merge block *)
        position_at_end merge_bb builder;
        build_phi [(then_val, then_bb); (else_val, else_bb)] "iftmp" builder

    | Typed_ast.Call (t, fname, args) ->
        let func_type = llvm_type t in
        let func = lookup_function fname the_module in
        let args = List.map (fun arg -> codegen_expr env arg) args in
        (match func with
         | Some f -> build_call func_type f (Array.of_list args) "calltmp" builder
         | None -> failwith ("Unknown function " ^ fname))

  (* Generate code for a block *)
  and codegen_block (Typed_ast.Block (_, exprs)) env =
    List.fold_left (fun _ expr -> codegen_expr env expr) (const_null (i32_type context)) exprs

  (* Generate code for a function *)
  let codegen_function (Typed_ast.TFunction (name, params, return_type, body)) env =
    let param_types = List.map (fun (TParam (t, _)) -> llvm_type t) params in
    let func_type = function_type (llvm_type return_type) (Array.of_list param_types) in
    let func = define_function name func_type the_module in
    let entry_bb = append_block context "entry" func in

    position_at_end entry_bb builder;
    List.iteri (fun i (TParam (_, pname)) ->
      let param = param func i in
      let alloca = build_alloca (type_of param) pname builder in
      ignore (build_store param alloca builder);
      Hashtbl.add named_values pname alloca) params;

    let body_val = codegen_block body env in
    ignore (build_ret body_val builder);
    func

  (* Generate code for the entire program *)
  let codegen_program (Typed_ast.Program (funcs, main_block)) =
    let env = StringMap.empty in
    List.iter (fun func -> ignore (codegen_function func env)) funcs;
    let _, main_block = codegen_block main_block  in
    (* return only the llvalue *)


    

  (* Print LLVM IR *)
  let print_ir () =
    dump_module the_module
end *)
