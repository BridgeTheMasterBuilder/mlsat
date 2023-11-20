open Common

type t
type key = int

(* val add : Clause.t -> t -> t *)
val add : int -> Clause.t -> t -> t
val empty : t
val find : key -> t -> Clause.t
val for_all : (key -> Clause.t -> bool) -> t -> bool
val get : key -> t -> Clause.t option
val is_empty : t -> bool

val remove_literal_from_clauses :
  Clause.elt -> IntSet.t -> t -> (t * Occurrence.t list, Clause.t) result

(* val remove_many : IntSet.t -> t -> t *)
val remove_many : IntSet.t -> t -> t * (Literal.t * int) list
val size : t -> int
val show : t -> string
