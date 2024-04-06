(* TODO t *)
type formula

val analyze_conflict : formula -> Clause.t -> Clause.t
val assignments : formula -> Literal.t list

val backtrack :
  formula -> Clause.t -> (formula * int, Clause.t * formula) Result.t
(* TODO result instead of Result.t *)

val choose_literal : formula -> Literal.t
val decision_level : formula -> int
val is_empty : formula -> bool
val learned_clauses : formula -> Clause.t list
val make_decision : formula -> (formula, Clause.t * formula) Result.t
val of_list : int -> int -> int list list -> formula option
val preprocess : formula -> formula
val restart : formula -> formula
val show : formula -> string

val unit_propagate :
  Literal.t * Clause.t -> formula -> (formula, Clause.t * formula) Result.t
