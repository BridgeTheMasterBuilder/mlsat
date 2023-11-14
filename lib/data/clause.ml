open Common
open Occurrence
module S = Iter.Set.Make (Literal)
include S

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
