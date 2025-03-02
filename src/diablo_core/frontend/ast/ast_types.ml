module Type = struct
  type t =
    | TVar of string
    | TInt
    | TBool
    | TString
    | TVoid
    | TFun of t list * t

  let rec to_string = function
    | TVar v -> v
    | TInt -> "int"
    | TBool -> "bool"
    | TString -> "string"
    | TVoid -> "void"
    | TFun (args, ret) ->
        Printf.sprintf "(%s -> %s)"
          (String.concat ", " (List.map to_string args))
          (to_string ret)
end

type param = TParam of Type.t * string
type scheme = Forall of int list * Type.t

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

type un_op = UnOpNot | UnOpNegate
