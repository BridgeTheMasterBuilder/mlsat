open Common
include Literal.Map

type t = IntSet.t Literal.Map.t
type key = Literal.t

let show o =
  fold
    (fun l cs s ->
      Printf.sprintf "%s%s:%s\n" s (Literal.show l)
        (IntSet.fold (fun l acc -> Printf.sprintf "%s%d " acc l) cs ""))
    o ""
