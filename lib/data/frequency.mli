module Map : sig
  type t
  type k = Literal.t
  type v = float

  val decay : t -> t
  val decr_iter : k Iter.iter -> t -> t
  val empty : unit -> t
  val flush_assigned : Assignment.Map.t -> t -> t
  val incr_iter : k Iter.iter -> t -> t
  val is_empty : t -> bool
  val mem : k -> t -> bool

  (* val merge : t -> t -> t *)
  val pop : t -> k

  (* val remove_literal : k -> t -> t *)
  val show : t -> string
  val to_iter : t -> (Literal.t * v) Iter.iter
end
