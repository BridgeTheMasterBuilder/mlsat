type t =
  | Decision of { literal : Literal.t; level : int }
  | Implication of {
      literal : Literal.t;
      level : int;
      implicant : Literal.t Array.t;
    }

type assignment = t

module Map : sig
  type key = Variable.t
  type t

  val add : key -> assignment -> t -> t
  val assignments : t -> Literal.t list
  val empty : t
  val find : key -> t -> assignment
  val find_opt : key -> t -> assignment option
  val mem : key -> t -> bool
  val to_iter : t -> (key * assignment) Iter.iter
  val value : Literal.t -> t -> Tribool.t
end

val level : t -> int
val literal : t -> Literal.t
val show : t -> string
val was_decided_on_level : t -> int -> bool
