type t
type elt = Literal.t

val make_occurrences : t -> int -> Occurrence.t list
val of_list : int list -> t
val remove : elt -> t -> t
val size : t -> int
val to_list : t -> Literal.t list
val to_set : t -> Literal.Set.t
