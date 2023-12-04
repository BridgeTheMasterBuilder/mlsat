type formula

val show : formula -> string
val add_clause : formula -> Clause.t -> Clause.t -> formula
val of_list : int list list -> formula
val is_empty : formula -> bool
val choose_literal : formula -> Literal.t
val unit_propagate : formula -> (formula, Clause.t * formula) Result.t
val rewrite : formula -> Literal.t -> formula
val analyze_conflict : formula -> Clause.t -> Clause.t
val backtrack : formula -> Clause.t -> formula * int
val restart : formula -> formula
val check_invariants : formula -> unit
