type elt = Literal.t
type t

val empty : t
val is_empty : t -> bool
val mem : elt -> t -> bool
val add : elt -> t -> t
val remove : elt -> t -> t
val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
val choose : t -> elt
val to_iter : t -> elt Iter.iter
val to_list : t -> elt list
val of_int_list : int list -> t
val size : t -> int
val show : t -> string
val to_set : t -> Literal.Set.t
