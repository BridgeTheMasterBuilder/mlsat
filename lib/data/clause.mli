type t
type elt = Literal.t

val make_occurrences : t -> int -> Occurrence.t list
val of_list : int list -> t
val remove : elt -> t -> t
val size : t -> int
