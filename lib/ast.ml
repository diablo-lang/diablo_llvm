type bin_op =
  | BinOpPlus
  | BinOpMinus
  | BinOpMult
  | BinOpDiv
  | BinOpRem
  | BinOpLessThan
  | BinOpGreaterThan
  | BinOpLessThanEqual
  | BinOpGreaterThanEqual
  | BinOpAnd
  | BinOpOr
  | BinOpEqual
  | BinOpNotEqual

type un_op =
  | UnOpNot
  | UnOpNegate

type expr =
  | Identifier of string
  | Integer of int
  | Boolean of bool
  | UnOp of un_op * expr
  | BinOp of bin_op * expr * expr
  | Let of string * expr
  | If of expr * block * block
  | Block of expr list
and
  block = Block of expr list

type program = Program of block

let rec pprint_ast expr =
  match expr with
  | Identifier s -> s
  | Integer i -> string_of_int i
  | Boolean b -> string_of_bool b
  | UnOp (UnOpNot, e) -> "(!" ^ pprint_ast e ^ ")"
  | UnOp (UnOpNegate, e) -> "(-" ^ pprint_ast e ^ ")"
  | BinOp (BinOpPlus, e1, e2) -> "(" ^ pprint_ast e1 ^ " + " ^ pprint_ast e2 ^ ")"
  | BinOp (BinOpMinus, e1, e2) -> "(" ^ pprint_ast e1 ^ " - " ^ pprint_ast e2 ^ ")"
  | BinOp (BinOpMult, e1, e2) -> "(" ^ pprint_ast e1 ^ " * " ^ pprint_ast e2 ^ ")"
  | BinOp (BinOpDiv, e1, e2) -> "(" ^ pprint_ast e1 ^ " / " ^ pprint_ast e2 ^ ")"
  | BinOp (BinOpRem, e1, e2) -> "(" ^ pprint_ast e1 ^ " % " ^ pprint_ast e2 ^ ")"
  | BinOp (BinOpLessThan, e1, e2) -> "(" ^ pprint_ast e1 ^ " < " ^ pprint_ast e2 ^ ")"
  | BinOp (BinOpGreaterThan, e1, e2) -> "(" ^ pprint_ast e1 ^ " > " ^ pprint_ast e2 ^ ")"
  | BinOp (BinOpLessThanEqual, e1, e2) -> "(" ^ pprint_ast e1 ^ " <= " ^ pprint_ast e2 ^ ")"
  | BinOp (BinOpGreaterThanEqual, e1, e2) -> "(" ^ pprint_ast e1 ^ " >= " ^ pprint_ast e2 ^ ")"
  | BinOp (BinOpAnd, e1, e2) -> "(" ^ pprint_ast e1 ^ " && " ^ pprint_ast e2 ^ ")"
  | BinOp (BinOpOr, e1, e2) -> "(" ^ pprint_ast e1 ^ " || " ^ pprint_ast e2 ^ ")"
  | BinOp (BinOpEqual, e1, e2) -> "(" ^ pprint_ast e1 ^ " == " ^ pprint_ast e2 ^ ")"
  | BinOp (BinOpNotEqual, e1, e2) -> "(" ^ pprint_ast e1 ^ " != " ^ pprint_ast e2 ^ ")"
  | Let (s, e) -> "let " ^ s ^ " = " ^ pprint_ast e ^ "\n"
  | If (e1, Block e2, Block e3) -> "if " ^ pprint_ast e1 ^ " then\n" ^ pprint_ast (Block e2) ^ "\nelse\n" ^ pprint_ast (Block e3) ^ "\n"
  | Block es -> "{\n" ^ String.concat "\n" (List.map (fun e -> "  " ^ pprint_ast e ^ "\n") es) ^ "}\n"