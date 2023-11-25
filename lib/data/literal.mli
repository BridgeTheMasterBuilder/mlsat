type t

module Map : Map.S with type key = t
module Set : Iter.Set.S with type elt = t

val compare : t -> t -> int
val equal : t -> t -> bool
val invalid : t
val is_negated : t -> bool
val neg : t -> t
val of_int : int -> t
val show : t -> string
val signum : t -> int
val var : t -> t
