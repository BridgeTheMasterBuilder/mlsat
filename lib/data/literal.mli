type t [@@deriving show]

module Map : Map.S with type key = t
module Set : Iter.Set.S with type elt = t

module List : sig
  val of_int_list : int list -> t list
end

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int
val neg : t -> t
val of_int_unchecked : int -> t
val of_var : Variable.t -> t
val show : t -> string
val signum : t -> int
val var : t -> Variable.t
