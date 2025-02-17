(* open Ast.Ast_types
open Typed_ast

exception TypeError of string

let type_error msg =
  let err = Printf.sprintf "Type error: %s" msg in
  failwith err

let bin_on_err = "Operator and operand type mismatch"

let un_op_err = "Operator and operand type mismatch"

let if_guard_err = "If condition must be boolean"

let if_branch_err = "If branches must have same type"

module TypeEnv = Map.Make(String)

type type_env = scheme TypeEnv.t

let lookup env x =
  match TypeEnv.find_opt x env with
  | Some (Forall (_, typ)) -> typ
  | None -> raise (TypeError ("Unbound variable: " ^ x))

(* Type Inference *)
let rec infer_expr (env: type_env) (expr: Parsed_ast.expr): Type.t =
  match expr with
  | Parsed_ast.Integer _ -> TInt
  | Parsed_ast.Boolean _ -> TBool
  | Parsed_ast.Identifier name -> lookup env name
  | Parsed_ast.BinOp (bin_op, e1, e2) -> infer_bin_op env bin_op e1 e2
  | Parsed_ast.UnOp (un_op, e) -> infer_un_op env un_op e
  | Parsed_ast.If (cond_expr, then_expr, else_expr) -> infer_if env cond_expr then_expr else_expr
  | Parsed_ast.Let (name, e) -> infer_let env name e
  | Parsed_ast.Call (name, args) -> infer_call env name args

and infer_bin_op env bin_op e1 e2 =
  match bin_op, infer_expr env e1, infer_expr env e2 with
  | BinOpPlus, TInt, TInt -> TInt
  | BinOpMinus, TInt, TInt -> TInt
  | BinOpMult, TInt, TInt -> TInt
  | BinOpDiv, TInt, TInt -> TInt
  | BinOpRem, TInt, TInt -> TInt
  | BinOpLessThan, TInt, TInt -> TBool
  | BinOpGreaterThan, TInt, TInt -> TBool
  | BinOpLessThanEqual, TInt, TInt -> TBool
  | BinOpGreaterThanEqual, TInt, TInt -> TBool
  | BinOpAnd, TBool, TBool -> TBool
  | BinOpOr, TBool, TBool -> TBool
  | BinOpEqual, TInt, TInt -> TBool
  | BinOpNotEqual, TInt, TInt -> TBool
  | _ -> raise (TypeError bin_on_err)

and infer_un_op env un_op e =
  match un_op, infer_expr env e with
  | UnOpNot, TBool -> TBool
  | UnOpNegate, TInt -> TInt
  | _ -> raise (TypeError un_op_err)

and infer_if env cond then_expr else_expr =
  let t1 = infer_expr env cond in
  let t2 = infer_expr env then_expr in
  let t3 = infer_expr env else_expr in
  if t1 <> TBool then
    raise (TypeError if_guard_err)
  else
    if t2 <> t3 then
      raise (TypeError if_branch_err)
    else
      t2

and infer_let env x e =
  let t = infer_expr env e in
  let _ = TypeEnv.add x (Forall ([], t)) env in
  t

and infer_call env name args =
  let arg_types = List.map (infer_expr env) args in
  let _ = List.iter (fun t -> if t <> TInt then raise (TypeError "Arguments must be integers")) arg_types in
  lookup env name *)

