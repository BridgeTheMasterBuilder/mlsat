open Common
open Occurrence
module S = Map.Make (Literal)
include S

type key = Literal.t
type t = (IntSet.t * IntSet.t) S.t
type occurrences = { occur1 : t; occur2 : t; occur_many : t }

let add_occurrence l cs m =
  let map, select, occurrence =
    if Literal.is_negated l then
      (Pair.map_snd, snd, (Literal.var l, (IntSet.empty, cs)))
    else (Pair.map_fst, fst, (Literal.var l, (cs, IntSet.empty)))
  in
  add_list_with ~f:(fun _ o -> map (IntSet.union (select o))) m [ occurrence ]

let add_occurrences =
  List.fold_left (fun ({ occur1; occur2; occur_many } as m) o ->
      match o with
      | Occur1 (l, cs) -> { m with occur1 = add_occurrence l cs occur1 }
      | Occur2 (l, cs) -> { m with occur2 = add_occurrence l cs occur2 }
      | OccurMany (l, cs) ->
          { m with occur_many = add_occurrence l cs occur_many })
(* TODO inline add_occurrence ? *)

let choose_opt m =
  choose_opt m
  |> Option.map (fun (l, (pos, neg)) ->
         if IntSet.cardinal pos > 1 then (Literal.neg l, IntSet.choose neg)
         else (l, IntSet.choose pos))

let get l m =
  get (Literal.var l) m
  |> Option.map (fun (pos, neg) -> if Literal.is_negated l then neg else pos)

let remove l { occur1; occur2; occur_many } =
  let occur1' = remove l occur1 in
  let occur2' = remove l occur2 in
  let occur_many' = remove l occur_many in
  { occur1 = occur1'; occur2 = occur2'; occur_many = occur_many' }
