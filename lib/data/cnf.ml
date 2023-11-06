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

  let negated l = l < 0
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
end

type assignment =
  | Decision of { literal : int; level : int }
  | Implication of { literal : int; level : int; implicant : Clause.t }

type formula = {
  clauses : ClauseMap.t;
  original_clauses : ClauseMap.t;
  unit_clauses : ClauseMap.t;
  two_literal_clauses : ClauseMap.t;
  occurrence_map : OccurrenceMap.t;
  decision_level : int;
  assignments : assignment IntMap.t;
  trail : (assignment * formula) list;
  database : IntSet.t list;
}

let rec combine_const c = List.map (fun x -> (c, x))

let of_list =
  let rec aux
      ({
         clauses;
         unit_clauses = uc;
         two_literal_clauses = tlc;
         occurrence_map = om;
         _;
       } as f) n = function
    | [] -> f
    | c :: cs ->
        let clauses' = ClauseMap.add_clause n (Clause.of_list c) clauses in
        let uc', tlc' =
          match List.length c with
          | 1 -> (ClauseMap.add_clause n (Clause.of_list c) uc, tlc)
          | 2 -> (uc, ClauseMap.add_clause n (Clause.of_list c) tlc)
          | _ -> (uc, tlc)
        in
        let om' =
          List.fold_left (fun m l -> OccurrenceMap.add_occurrence l n m) om c
        in
        let f' =
          {
            f with
            clauses = clauses';
            original_clauses = clauses';
            unit_clauses = uc';
            two_literal_clauses = tlc';
            occurrence_map = om';
          }
        in
        aux f' (n + 1) cs
  in
  aux
    {
      clauses = ClauseMap.empty;
      original_clauses = ClauseMap.empty;
      unit_clauses = ClauseMap.empty;
      two_literal_clauses = ClauseMap.empty;
      occurrence_map = OccurrenceMap.empty;
      decision_level = 0;
      assignments = IntMap.empty;
      trail = [];
      database = [];
    }

let is_empty { clauses; _ } = IntMap.is_empty clauses

(* let choose_literal {occurrence_map_2;occurrence_map_n;  _} = *)
(*   let v = fst *)
