type key = Literal.t * Clause.t

(* type t = key Queue.t *)
type t

val clear : t -> t
val empty : t
val fold : ('a -> key -> 'a) -> 'a -> t -> 'a
val snoc : t -> key -> t
val take_front : t -> (key * t) option
