module Map : sig
  type t
  type k = Literal.t

  val add_many : Clause.t -> t -> t
  val decay : t -> t
  val empty : t
  val is_empty : t -> bool
  val mem : k -> t -> bool
  val pop : t -> k option
  val remove_literal : k -> t -> t
  val show : t -> string
  val to_iter : t -> (Literal.t * float) Iter.iter
end
