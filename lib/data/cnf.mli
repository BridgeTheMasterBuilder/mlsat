type formula

val add_clause : formula -> Clause.t -> formula
val analyze_conflict : formula -> Clause.t -> Clause.t
val backtrack : formula -> Clause.t -> formula * int

(* val check_invariants : formula -> unit *)
val choose_literal : formula -> Literal.t
val is_empty : formula -> bool
val make_decision : formula -> formula
val of_list : int -> int -> int list list -> formula
val preprocess : formula -> formula
val restart : formula -> formula

(* val show : formula -> string *)
val unit_propagate : formula -> (formula, Clause.t * formula) Result.t