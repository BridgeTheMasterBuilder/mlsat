type t

type clause = t

val empty : t

val equal : t -> t -> bool

val hash : t -> int

val of_list : int -> Literal.t list -> t

val of_iter : int -> Literal.t Iter.iter -> t

val show : t -> string

val size : t -> int

val to_array : t -> Literal.t array

val to_iter : t -> Literal.t Iter.iter

val unsafe_get : t -> int -> Literal.t

module Set : sig
  type elt = clause

  type t

  val add : elt -> t -> t

  val empty : unit -> t

  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a

  val remove : elt -> t -> t

  val show : t -> string
end
