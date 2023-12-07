type t = Clause.t Common.IntMap.t
type key = int

val add : key -> Clause.t -> t -> t
val choose_opt : t -> (key * Clause.t) option
val empty : t
val find : key -> t -> Clause.t
val find_opt : key -> t -> Clause.t option
val mem : key -> t -> bool
val is_empty : t -> bool
val remove : key -> t -> t
val show : t -> string
val size : t -> key
val to_iter : t -> (key * Clause.t) Iter.iter
