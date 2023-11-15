type t

module Map : Map.S with type key = t

(* module Set : sig *)
(*   include Iter.Set.S with type elt = t *)

(*   val to_int_list : t -> int list *)
(*   val to_int_set : t -> IntSet.t *)
(* end *)
module Set : Iter.Set.S with type elt = t

val compare : t -> t -> int
val invalid : t
val is_negated : t -> bool
val neg : t -> t
val of_int : int -> t
val var : t -> t
