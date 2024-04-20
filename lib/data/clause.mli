type t
type clause = t

module Watched : sig
  type t
  type watched_clause = t

  module Set : sig
    type elt = watched_clause
    type t

    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  end

  module Map : sig
    type t
    type key = Literal.t

    val find_opt : key -> t -> Set.t option
    val is_empty : t -> bool
    val make : int -> t
    val show : t -> string
  end

  type update_result =
    | WatchedLiteralChange of Map.t
    | Unit of (Literal.t * clause)
    | Falsified of clause
    | NoChange

  val to_clause : t -> clause
  val unwatch_clause : clause -> Map.t -> Map.t
  val update : Literal.t -> Assignment.Map.t -> t -> Map.t -> update_result
  val watch_clause : Assignment.Map.t -> clause -> Map.t -> update_result
end

val of_list : Literal.t list -> t
val of_iter : Literal.t Iter.iter -> t
val show : t -> string
val size : t -> int
val to_array : t -> Literal.t array
val to_iter : t -> Literal.t Iter.iter

module Set : sig
  include Iter.Set.S with type elt = clause

  val empty : unit -> t
  val show : t -> string
end
