module type S = sig
  type elt
  type t
  type elt_set

  val empty : t
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val is_empty : t -> bool
  val of_iter : elt Iter.iter -> t
  val pop : t -> (elt * t) option
  val pop_exn : t -> elt * t
  val push : elt -> t -> t
  val push_iter : elt Iter.iter -> t -> t
end

module Make (Ord : Set.OrderedType) : S with type elt = Ord.t
