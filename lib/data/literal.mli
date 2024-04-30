type t

module Map : Map.S with type key = t

module Set : Iter.Set.S with type elt = t

module List : sig
  val of_int_list : int list -> t list
end

module Array : sig
  val to_int_array : t array -> int array
end

val invalid : t

val compare : t -> t -> int

val equal : t -> t -> bool

val hash : t -> int

val neg : t -> t

val of_int_unchecked : int -> t

val of_var : Variable.t -> t

val same_polarity : t -> t -> bool

val show : t -> string

val signum : t -> int

val to_int : t -> int

val var : t -> Variable.t
