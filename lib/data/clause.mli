type t
type clause = t

module Watched : sig
  (* type t *)
  module Clause : sig
    (* TODO *)
    type t = {
      id : int;
      clause : clause;
      size : int;
      mutable index : int;
      mutable watched_literals : Literal.t * Literal.t;
    }
  end

  type t = Clause.t
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
    | WatchedLiteralChange of (watched_clause * Map.t)
    | Unit of (Literal.t * clause)
    | Falsified of clause
    | NoChange

  val fold : ('a -> Literal.t -> 'a) -> 'a -> t -> 'a
  val watch_clause : Assignment.Map.t -> clause -> int -> Map.t -> update_result
  val unwatch_clause : clause -> Map.t -> Map.t
  val update : Literal.t -> Assignment.Map.t -> t -> Map.t -> update_result
end

module Map : sig
  type t
  type key = int

  val add : Watched.watched_clause -> t -> t
  val find : key -> t -> Watched.watched_clause
  val is_empty : t -> bool
  val make : int -> t
  val remove : int -> t -> t
  val show : t -> string
  val size : t -> key
  val to_iter : t -> (key * Watched.watched_clause) Iter.iter
end

val empty : t
val of_list : Literal.t list -> t
val of_array : Literal.t array -> t
val show : t -> string
val size : t -> int
val to_array : t -> Literal.t array
val to_iter : t -> Literal.t Iter.iter
