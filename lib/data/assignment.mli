type t =
  | Decision of { literal : Literal.t; level : int }
  | Implication of { literal : Literal.t; level : int; implicant : Clause.t }

type assignment = t

module Map : sig
  (* include module type of Literal.Map *)

  type key = Literal.t
  type t

  val add : key -> assignment -> t -> t
  val empty : t
  val find_opt : key -> t -> assignment option
  val to_iter : t -> (key * assignment) Iter.iter
end

val level : t -> int
val literal : t -> Literal.t
val show : t -> string
val was_decided_on_level : t -> int -> bool
