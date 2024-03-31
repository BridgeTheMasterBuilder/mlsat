type elt = Literal.t
type t
type clause = t

module Map : sig
  type t
  type key = int

  val add : clause -> t -> t
  val find : key -> t -> clause
  val is_empty : t -> bool
  val make : int -> t
  val show : t -> string
  val size : t -> key
  val to_iter : t -> (key * clause) Iter.iter
end

val add : elt -> t -> t
val equal : t -> t -> bool
val empty : t
val filter : (elt -> bool) -> t -> t
val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
val is_empty : t -> bool
val mem : elt -> t -> bool
val of_int_list : int list -> t
val of_iter : elt Iter.iter -> t
val remove : elt -> t -> t
val show : t -> string
val size : t -> int
val to_iter : t -> elt Iter.iter
val to_list : t -> elt list
