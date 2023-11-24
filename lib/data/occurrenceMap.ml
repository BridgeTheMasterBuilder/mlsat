open Common
open Occurrence
module S = Map.Make (Literal)
include S

type key = Literal.t
type t = (IntSet.t * IntSet.t) S.t
type occurrences = { occur1 : t; occur2 : t; occur_n : t }

let add_occurrence l cs m =
  let map, select, occurrence =
    if Literal.is_negated l then
      (Pair.map_snd, snd, (Literal.var l, (IntSet.empty, cs)))
    else (Pair.map_fst, fst, (Literal.var l, (cs, IntSet.empty)))
  in
  add_list_with ~f:(fun _ o -> map (IntSet.union (select o))) m [ occurrence ]

let add_occurrences =
  List.fold_left (fun ({ occur1; occur2; occur_n } as m) o ->
      match o with
      | Occur1 (l, cs) ->
          {
            m with
            occur1 = add_occurrence l cs occur1;
            occur_n = add_occurrence l cs occur_n;
          }
      | Occur2 (l, cs) ->
          {
            m with
            occur2 = add_occurrence l cs occur2;
            occur_n = add_occurrence l cs occur_n;
          }
      | OccurN (l, cs) -> { m with occur_n = add_occurrence l cs occur_n })
(* TODO inline add_occurrence ? *)

let choose_opt m =
  choose_opt m
  |> Option.flat_map (fun (l, (pos, neg)) ->
         if IntSet.cardinal pos > 1 then Some (Literal.neg l, IntSet.choose neg)
         else IntSet.choose_opt pos |> Option.map (fun c -> (l, c)))

let find l m =
  let pos, neg = find (Literal.var l) m in
  if Literal.is_negated l then neg else pos

(* let get l m = *)
(*   get (Literal.var l) m *)
(*   |> Option.map (fun (pos, neg) -> if Literal.is_negated l then neg else pos) *)
(* TODO get_pos get_neg *)
let get l m =
  (* get (Literal.var l) m |> Option.map (fun (pos, neg) -> IntSet.union pos neg) *)
  get (Literal.var l) m

let remove l { occur1; occur2; occur_n } =
  let occur1' = remove l occur1 in
  let occur2' = remove l occur2 in
  let occur_n' = remove l occur_n in
  { occur1 = occur1'; occur2 = occur2'; occur_n = occur_n' }

let remove_occurrence l c m =
  let map, select, occurrence =
    if Literal.is_negated l then
      (Pair.map_snd, snd, (Literal.var l, (IntSet.empty, IntSet.singleton c)))
    else (Pair.map_fst, fst, (Literal.var l, (IntSet.singleton c, IntSet.empty)))
  in
  (* print_endline ("Deleting " ^ string_of_int c ^ " from " ^ Literal.show l); *)
  add_list_with
    ~f:(fun k o ->
      map (fun s ->
          (* print_endline *)
          (*   (Literal.show k ^ ": " ^ IntSet.show s ^ " \\ " *)
          (*   ^ IntSet.show (select o) *)
          (*   ^ " = " *)
          (*   ^ IntSet.show (IntSet.diff s (select o))); *)
          IntSet.diff s (select o)))
    m [ occurrence ]

let remove_occurrences =
  List.fold_left (fun ({ occur1; occur2; occur_n } as m) (l, c) ->
      let m' = { m with occur_n = remove_occurrence l c occur_n } in
      if mem (Literal.var l) occur2 then
        { m' with occur2 = remove_occurrence l c occur2 }
      else { m' with occur1 = remove_occurrence l c occur1 })

let show m = ""
(* fold *)
(*   (fun l (pos, neg) s -> *)
(*     s ^ Literal.show l ^ " -> (" ^ IntSet.show pos ^ ", " ^ IntSet.show neg *)
(*     ^ ")\n") *)
(*   m "" *)

let show_occurrences { occur1; occur2; occur_n } =
  "1-occurrences:\n" ^ show occur1 ^ "\n2-occurrences:\n" ^ show occur2
  ^ "\nMany occurrences:\n" ^ show occur_n

let update_occurrence l cs m =
  let map, select, occurrence =
    if Literal.is_negated l then
      (Pair.map_snd, snd, (Literal.var l, (IntSet.empty, cs)))
    else (Pair.map_fst, fst, (Literal.var l, (cs, IntSet.empty)))
  in
  add_list_with ~f:(fun _ o -> map (fun _ -> select o)) m [ occurrence ]

let update_occurrences =
  List.fold_left (fun ({ occur1; occur2; occur_n } as m) o ->
      match o with
      | Occur1 (l, cs) ->
          {
            occur1 = update_occurrence l cs occur1;
            occur2 = S.remove l occur2;
            occur_n = update_occurrence l cs occur_n;
          }
      | Occur2 (l, cs) ->
          {
            m with
            occur2 = update_occurrence l cs occur2;
            occur_n = update_occurrence l cs occur_n;
          }
      | OccurN (l, cs) -> { m with occur_n = update_occurrence l cs occur_n })
