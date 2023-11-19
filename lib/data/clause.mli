type t
type elt = Literal.t

val add : elt -> t -> t
val empty : t
val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
val for_all : (elt -> bool) -> t -> bool
val make_occurrences : t -> int -> Occurrence.t list
val mem : elt -> t -> bool
val of_list : int list -> t
val remove : elt -> t -> t
val size : t -> int
val show : t -> string
val to_iter : t -> elt Iter.t
val to_list : t -> Literal.t list
val to_set : t -> Literal.Set.t
