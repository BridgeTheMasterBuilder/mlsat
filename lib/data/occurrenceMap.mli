open Common

type t
type occurrences = { occur1 : t; occur2 : t; occur_many : t }
type key = Literal.t

val add_occurrences : occurrences -> Occurrence.t list -> occurrences
val choose_opt : t -> (key * int) option
val empty : t
val fold : (key -> IntSet.t * IntSet.t -> 'a -> 'a) -> t -> 'a -> 'a
val get : key -> t -> IntSet.t option
val is_empty : t -> bool
