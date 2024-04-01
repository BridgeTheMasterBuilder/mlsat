module Map : sig
  type t
  type k = Literal.t

  val decay : t -> t
  val decr_iter : k Iter.iter -> t -> t
  val empty : t
  val incr_iter : k Iter.iter -> t -> t
  val is_empty : t -> bool
  val mem : k -> t -> bool
  val pop : t -> k
  val remove_literal : k -> t -> t
  val show : t -> string
  val to_iter : t -> (Literal.t * float) Iter.iter
end
