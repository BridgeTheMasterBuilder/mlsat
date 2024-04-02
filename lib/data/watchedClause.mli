type t
type watched_clause = t

module Set : sig
  type elt = watched_clause
  type t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
end

module Map : sig
  type t
  type key = Literal.t

  val add : key -> watched_clause -> t -> t
  val find_opt : key -> t -> Set.t option
  val fold : (key -> Set.t -> 'a -> 'a) -> t -> 'a -> 'a
  val is_empty : t -> bool
  val make : int -> t
  val mem : key -> t -> bool
  val remove : key -> int -> t -> t
  val show : t -> string
  val update : t -> f:(key -> Set.t option -> Set.t option) -> k:key -> unit
end

type update_result =
  | WatchedLiteralChange of Map.t
  | Unit of (int * Clause.t)
  | Falsified of (int * Clause.t)
  | NoChange

val clause : t -> int * Clause.t
val fold : ('a -> Literal.t -> 'a) -> 'a -> t -> 'a
val of_clause : Assignment.Map.t -> Clause.t -> int -> t option
val update : Literal.t -> Assignment.Map.t -> t -> Map.t -> update_result
val watched_literals : t -> Literal.t * Literal.t
