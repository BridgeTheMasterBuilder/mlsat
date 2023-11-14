open Common

type t
type key = int

val add : Clause.t -> t -> t
val empty : t
val find : key -> t -> Clause.t
val is_empty : t -> bool

val remove_literal_from_clauses :
  Clause.elt -> IntSet.t -> t -> (t * Occurrence.t list, Clause.t) result
