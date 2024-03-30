type t =
  | Decision of { literal : Literal.t; level : int }
  | Implication of { literal : Literal.t; level : int; implicant : Clause.t }

type assignment = t

module Map : sig
  type key = Literal.t
  type t

  val add : key -> assignment -> t -> t
  val assignments : t -> Literal.t list
  val empty : t
  val find_opt : key -> t -> assignment option
  val mem : key -> t -> bool
  val to_iter : t -> (key * assignment) Iter.iter
  val value : key -> t -> Tribool.t
end

val level : t -> int
val literal : t -> Literal.t
val show : t -> string
val was_decided_on_level : t -> int -> bool
