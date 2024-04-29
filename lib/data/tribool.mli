type t

val unknown : t

val is_false : t -> bool

val is_known : t -> bool

val is_nonfalse : t -> bool

val is_nontrue : t -> bool

val is_true : t -> bool

val is_unknown : t -> bool

val of_bool : bool -> t

val of_bool_opt : bool option -> t

val of_int : int -> t

(* val to_bool_opt : t -> bool option *)
