open Common

type t =
  | Occur1 of (Literal.t * IntSet.t)
  | Occur2 of (Literal.t * IntSet.t)
  | OccurN of (Literal.t * IntSet.t)
