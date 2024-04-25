module L = Literal

type watched_clause

module ClauseSet : sig
  type elt = watched_clause
  type t

  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
end

module Literal : sig
  module Map : sig
    type t
    type key = Literal.t

    val find_opt : key -> t -> ClauseSet.t option
    val is_empty : t -> bool
    val make : int -> t
    val show : t -> string
  end
end

module Clause : sig
  type t = watched_clause

  module Set = ClauseSet

  type update_result =
    | WatchedLiteralChange of Literal.Map.t
    | Unit of (L.t * Clause.t)
    | Falsified of Clause.t
    | NoChange

  val to_clause : t -> Clause.t
  val unwatch : Clause.t -> Literal.Map.t -> Literal.Map.t
  val update : L.t -> Assignment.Map.t -> t -> Literal.Map.t -> update_result
  val watch : Assignment.Map.t -> Clause.t -> Literal.Map.t -> update_result
end