module type S = sig
  type elt
  type t
  type elt_set

  val is_empty : t -> bool
  val of_iter : elt Iter.iter -> t
  val pop : t -> (elt * t) option
  val push : elt -> t -> t
  val push_iter : elt Iter.iter -> t -> t
end

module Make (Ord : Set.OrderedType) : S with type elt = Ord.t
