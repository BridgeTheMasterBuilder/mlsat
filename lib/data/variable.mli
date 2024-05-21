type t

module Map : Map.S with type key = t

module Set : Iter.Set.S with type elt = t

val compare : t -> t -> int

val equal : t -> t -> bool

val hash : t -> int

val of_int_unchecked : int -> t

val show : t -> string

val to_int : t -> int
