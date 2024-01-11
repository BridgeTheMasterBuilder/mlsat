open Common

type t = IntSet.t
type occurrence = t

module Map : sig
  type t
  type key = Literal.t

  val add : key -> occurrence -> t -> t
  val empty : t
  val find : key -> t -> occurrence
  val find_opt : key -> t -> occurrence option
  val is_empty : t -> bool
  val mem : key -> t -> bool
  val show : t -> string
  val to_iter : t -> (key * occurrence) Iter.iter
  val update : key -> (occurrence option -> occurrence option) -> t -> t
end
