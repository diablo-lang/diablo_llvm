open Ast.Ast_types

module type StaticEnvironment = sig
  (** [t] is the type of a static environment. *)
  type t

  (** [empty] is the empty static environment. *)
  val empty : t

  (** [lookup env x] gets the binding of [x] in [env].
      Raises: [Failure] if [x] is not bound in [env]. *)
  val lookup : t -> string -> diablo_type

  (** [extend env x ty] is [env] extended with a binding
      of [x] to [ty]. *)
  val extend : t -> string -> diablo_type -> t
end

module StaticEnvironment : StaticEnvironment = struct
  type t = (string * diablo_type) list

  let empty = []

  let lookup env x =
    try List.assoc x env
    with Not_found -> failwith "Unbound variable"
  
  let extend env x ty =
    (x, ty) :: env
end