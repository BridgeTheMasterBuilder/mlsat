module Map : sig
  type t
  type k = Literal.t

  val empty : t
  val is_empty : t -> bool
  val pop : t -> k option
  val add_many : Clause.t -> t -> t
  val decay : t -> t
  val remove_literal : k -> t -> t
  val show : t -> string
end
