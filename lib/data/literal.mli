type t

val compare : t -> t -> int
val invalid : t
val is_negated : t -> bool
val neg : t -> t
val of_int : int -> t
val var : t -> t
