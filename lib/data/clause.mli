type elt = Literal.t
type t
type clause = t

module Map : sig
  type t
  type key = int

  val add : clause -> t -> t
  val empty : t
  val find : key -> t -> clause
  val is_empty : t -> bool
  val show : t -> string
  val size : t -> key
  val to_iter : t -> (key * clause) Iter.iter
end

val empty : t
val is_empty : t -> bool
val mem : elt -> t -> bool
val add : elt -> t -> t
val equal : t -> t -> bool
val remove : elt -> t -> t
val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
val to_iter : t -> elt Iter.iter
val to_list : t -> elt list
val of_int_list : int list -> t
val of_iter : elt Iter.iter -> t
val size : t -> int
val show : t -> string
