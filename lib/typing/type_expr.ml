open Ast.Ast_types
open Parsing

let unbound_var_err = "Unbound variable"

let type_error msg =
  let err = Printf.sprintf "Type error: %s" msg in
  failwith err

let bin_on_err = "Operator and operand type mismatch"

let un_op_err = "Operator and operand type mismatch"

let if_guard_err = "If condition must be boolean"

let if_branch_err = "If branches must have same type"

let empty = []

let lookup env e =
  match List.assoc_opt e env with
  | Some ty -> ty
  | None -> failwith unbound_var_err

let rec typeof env = function
  | Parsed_ast.Integer _ -> TInt
  | Parsed_ast.Boolean _ -> TBool
  | Parsed_ast.Identifier x -> lookup env x
  | Parsed_ast.BinOp (bin_op, e1, e2) -> typeof_bin_op env bin_op e1 e2
  | Parsed_ast.UnOp (un_op, e) -> typeof_un_op env un_op e
  | _ -> failwith "Not implemented"

and typeof_bin_op env bin_op e1 e2 =
  match bin_op, typeof env e1, typeof env e2 with
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
  | BinOpEqual, _, _ -> TBool
  | BinOpNotEqual, _, _ -> TBool
  | _ -> type_error bin_on_err

and typeof_un_op env un_op e =
  match un_op, typeof env e with
  | UnOpNot, TBool -> TBool
  | UnOpNegate, TInt -> TInt
  | _ -> type_error un_op_err

and typeof_if env e1 e2 e3 =
  let t1 = typeof env e1 in
  if t1 <> TBool then
    type_error if_guard_err
  else
    let t2 = typeof env e2 in
    let t3 = typeof env e3 in
    if t2 <> t3 then
      type_error if_branch_err
    else
      t2


let typecheck e =
  ignore (typeof empty e); e

let type_expr function_defns (expr: Parsed_ast.expr) env =
  let open Result in
  match expr with
  | Parsed_ast.Integer (i) -> Ok (Typed_ast.Integer i, TInt)
  | Parsed_ast.Boolean (b) -> Ok (Typed_ast.Boolean b, TBool)