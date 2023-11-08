open Containers
module IntMap : Map.S
module IntSet : Iter.Set.S

module Literal : sig
  type t

  val compare : t -> t -> int
  val var : t -> t
  val neg : t -> t
  val is_negated : t -> bool
  val invalid : t
  val of_int : int -> t
end

module Clause : sig
  type t

  val of_list : int list -> t
  val make_occurrences : t -> int -> (Literal.t * int) list
  val size : t -> int
end

(* module ClauseMap : sig *)
(*   type t *)

(*   val add_clause : int -> Clause.t -> t -> t *)
(*   val empty : t *)
(*   val is_empty : t -> bool *)
(* end *)

(* module OccurrenceMap : sig *)
(*   type t *)
(*   type key = Literal.t *)

(*   val add_occurrences : t -> (key * int) list -> t *)
(*   val empty : t *)
(*   val is_empty : t -> bool *)
(*   val fold : (key -> IntSet.t * IntSet.t -> 'a -> 'a) -> t -> 'a -> 'a *)
(* end *)

(* type assignment = *)
(*   | Decision of { literal : int; level : int } *)
(*   | Implication of { literal : int; level : int; implicant : Clause.t } *)

type formula

val of_list : int list list -> formula
val is_empty : formula -> bool
val choose_literal : formula -> Literal.t
val unit_propagate : formula -> (formula, Clause.t * formula) result
