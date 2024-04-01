type t

val is_false : t -> bool
val is_known : t -> bool
val is_nonfalse : t -> bool
val is_nontrue : t -> bool
val is_true : t -> bool
val is_unknown : t -> bool
val of_bool_opt : bool option -> t
val to_bool_opt : t -> bool option
