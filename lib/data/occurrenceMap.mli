open Common

type t
type key = Literal.t

val add : key -> IntSet.t -> t -> t
val choose : t -> key * IntSet.t
val empty : t
val find : key -> t -> IntSet.t
val find_opt : key -> t -> IntSet.t option
val is_empty : t -> bool
val remove : key -> t -> t
val show : t -> string
val to_iter : t -> (key * IntSet.t) Iter.iter
val update : key -> (IntSet.t option -> IntSet.t option) -> t -> t
