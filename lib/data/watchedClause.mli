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
  val create : int -> t
  val find_opt : key -> t -> Set.t option
  val mem : key -> t -> bool
  val fold : (key -> Set.t -> 'a -> 'a) -> t -> 'a -> 'a
  val update : t -> f:(key -> Set.t option -> Set.t option) -> k:key -> unit
  val empty : t
  val is_empty : t -> bool
  val remove : key -> watched_clause -> t -> t
  val show : t -> string
end

val clause : t -> int * Clause.t
val fold : ('a -> Literal.t -> 'a) -> 'a -> t -> 'a
val of_clause : Assignment.Map.t -> Clause.t -> int -> t

type update_result =
  | WatcherChange of (Literal.t * Literal.t * Literal.t * t)
  | Unit of t
  | Falsified of t
  | NoChange

val update : Literal.t -> Assignment.Map.t -> t -> update_result
val watched_literals : t -> (Literal.t * Literal.t) option
