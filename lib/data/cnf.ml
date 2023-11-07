open Containers
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

module Clause = struct
  include IntSet
end

module ClauseMap = struct
  include IntMap

  type t = Clause.t IntMap.t

  let add_clause n clause = add n clause
end

module Literal = struct
  include Int

  let var = abs
  let negated l = l < 0
  let invalid = 0
end

module OccurrenceMap = struct
  include IntMap

  type t = (IntSet.t * IntSet.t) IntMap.t

  let add_occurrence l n m =
    let map, select, occurrence =
      if Literal.negated l then
        (Pair.map_snd, snd, (l, (IntSet.empty, IntSet.singleton n)))
      else (Pair.map_fst, fst, (l, (IntSet.singleton n, IntSet.empty)))
    in
    add_list_with ~f:(fun _ o -> map (IntSet.union (select o))) m [ occurrence ]

  let add_occurrences = List.fold_left (fun m (l, c) -> add_occurrence l c m)
end

type assignment =
  | Decision of { literal : int; level : int }
  | Implication of { literal : int; level : int; implicant : Clause.t }

type formula = {
  clauses : ClauseMap.t;
  original_clauses : ClauseMap.t;
  occurrence_map_1 : OccurrenceMap.t;
  occurrence_map_2 : OccurrenceMap.t;
  occurrence_map_n : OccurrenceMap.t;
  decision_level : int;
  assignments : assignment IntMap.t;
  trail : (assignment * formula) list;
  database : IntSet.t list;
}

let combine_const ls c = List.map (fun x -> (x, c)) ls

let of_list =
  let rec aux
      ({
         clauses;
         occurrence_map_1 = uc;
         occurrence_map_2 = tlc;
         occurrence_map_n = om;
         _;
       } as f) n = function
    | [] -> f
    | c :: cs ->
        let clauses' = ClauseMap.add_clause n (Clause.of_list c) clauses in
        let uc', tlc' =
          match List.length c with
          | 1 -> (OccurrenceMap.add_occurrences uc (combine_const c n), tlc)
          | 2 -> (uc, OccurrenceMap.add_occurrences tlc (combine_const c n))
          | _ -> (uc, tlc)
        in
        let om' = OccurrenceMap.add_occurrences om (combine_const c n) in
        let f' =
          {
            f with
            clauses = clauses';
            original_clauses = clauses';
            occurrence_map_1 = uc';
            occurrence_map_2 = tlc';
            occurrence_map_n = om';
          }
        in
        aux f' (n + 1) cs
  in
  aux
    {
      clauses = ClauseMap.empty;
      original_clauses = ClauseMap.empty;
      occurrence_map_1 = OccurrenceMap.empty;
      occurrence_map_2 = OccurrenceMap.empty;
      occurrence_map_n = OccurrenceMap.empty;
      decision_level = 0;
      assignments = IntMap.empty;
      trail = [];
      database = [];
    }

let is_empty { clauses; _ } = IntMap.is_empty clauses

let choose_literal { occurrence_map_2 = tlc; occurrence_map_n = om; _ } =
  let m = if OccurrenceMap.is_empty tlc then om else tlc in
  OccurrenceMap.fold
    (fun l (pos, neg) (l', m) ->
      let size_pos = IntSet.cardinal pos in
      let size_neg = IntSet.cardinal neg in
      let occurrences = size_pos + size_neg in
      let l =
        if size_pos < size_neg then Literal.(neg (var l)) else Literal.var l
      in
      if occurrences > m then (l, occurrences) else (l', m))
    m (Literal.invalid, 0)
  |> fst
