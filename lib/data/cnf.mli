type formula

val add_clause : formula -> Clause.t -> Clause.t -> formula
val analyze_conflict : formula -> Clause.t -> Clause.t
val backtrack : formula -> Clause.t -> formula * int
val check_invariants : formula -> unit
val choose_literal : formula -> Literal.t
val is_empty : formula -> bool
val of_list : int list list -> formula
val preprocess : formula -> formula
val restart : formula -> formula
val rewrite : formula -> Literal.t -> formula
val show : formula -> string
val unit_propagate : formula -> (formula, Clause.t * formula) Result.t
