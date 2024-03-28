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
end

val is_false : Literal.t -> Map.t -> bool
val is_true : Literal.t -> Map.t -> bool
val is_undefined : Literal.t -> Map.t -> bool
val level : t -> int
val literal : t -> Literal.t
val show : t -> string
val was_decided_on_level : t -> int -> bool
