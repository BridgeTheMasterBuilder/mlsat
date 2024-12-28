type t =
  | Decision of {literal: Literal.t; level: int}
  | Implication of {literal: Literal.t; level: int; implicant: Clause.t}

type assignment = t

module Map : sig
  type key = Variable.t

  type t

  val add : key -> assignment -> t -> t

  val assignments : t -> Literal.t list

  val empty : unit -> t

  val find : key -> t -> assignment

  val find_opt : key -> t -> assignment option

  val mem : key -> t -> bool

  val size : t -> int

  val value : Literal.t -> t -> Tribool.t

  module Cached : sig
    type uncached = t

    type key := Variable.t

    type t

    val add : key -> assignment -> t -> t

    val clear : t -> t

    val make : int -> t

    val mem : key -> t -> bool

    val refresh : t -> uncached -> t

    val value : Literal.t -> t -> Tribool.t
  end
end

val compare : t -> t -> int

val level : t -> int

val literal : t -> Literal.t

val show : t -> string

val was_decided_on_level : int -> t -> bool
