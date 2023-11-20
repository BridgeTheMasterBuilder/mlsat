open Common

type t
type occurrences = { occur1 : t; occur2 : t; occur_n : t }
type key = Literal.t

val add_occurrences : occurrences -> Occurrence.t list -> occurrences
val choose_opt : t -> (key * int) option
val empty : t
val find : key -> t -> IntSet.t
val fold : (key -> IntSet.t * IntSet.t -> 'a -> 'a) -> t -> 'a -> 'a
val for_all : (key -> IntSet.t * IntSet.t -> bool) -> t -> bool

(* val get : key -> t -> IntSet.t option *)
val get : key -> t -> (IntSet.t * IntSet.t) option
val is_empty : t -> bool

(* val remove_occurrences : occurrences -> Occurrence.t list -> occurrences *)
val remove : key -> occurrences -> occurrences
val remove_occurrences : occurrences -> (key * int) list -> occurrences
val show_occurrences : occurrences -> string
val update_occurrences : occurrences -> Occurrence.t list -> occurrences
