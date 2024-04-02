type t
type clause = t

module Map : sig
  type t
  type key = int

  val add : clause -> t -> t
  val find : key -> t -> clause
  val is_empty : t -> bool
  val make : int -> t
  val remove : int -> t -> t
  val show : t -> string
  val size : t -> key
  val to_iter : t -> (key * clause) Iter.iter
end

val empty : t
val of_list : Literal.t list -> t
val show : t -> string
val size : t -> int

(* val to_array : t -> Literal.t Array.t *)
val to_iter : t -> Literal.t Iter.iter
val to_list : t -> Literal.t list
