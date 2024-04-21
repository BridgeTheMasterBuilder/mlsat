type t

val analyze_conflict : t -> Clause.t -> Clause.t
val assignments : t -> Literal.t list
val backtrack : t -> Clause.t -> (t * int, Clause.t * t) result
val choose_literal : t -> Literal.t
val decision_level : t -> int
val is_empty : t -> bool
val trace : t -> Database.trace
val make_decision : t -> (t, Clause.t * t) result
val of_list : int -> int -> int list list -> t option
val preprocess : t -> t
val restart : t -> t
val simplify : t -> t
val show : t -> string
val check : t -> unit
