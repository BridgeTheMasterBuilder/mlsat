open Common
open Occurrence
include Literal.Set

let make_occurrences ls c =
  Iter.(
    to_iter ls
    |> map (fun l ->
           match cardinal ls with
           | 1 -> Occur1 (l, IntSet.singleton c)
           | 2 -> Occur2 (l, IntSet.singleton c)
           | _ -> OccurN (l, IntSet.singleton c))
    |> to_list)

let of_list ls = Iter.(of_list ls |> map Literal.of_int |> of_iter)
let size = cardinal

let show c =
  "("
  ^ fold
      (fun l s ->
        (if String.equal s "" then "" else s ^ "\\/") ^ Literal.show l)
      c ""
  ^ ")"

let to_set = Fun.id
