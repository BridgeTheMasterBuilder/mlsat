open Containers
module IntMap = CCIntMap

module IntMultiMap = struct
  include CCMultiMap.Make (Int) (Int)

  let add_all mm k = List.fold_left (fun m l -> add m k l) mm
end

module IntSet = Set.Make (Int)

type literal = int

type assignment =
  | Decision of { literal : int; level : int }
  | Implication of { literal : int; level : int; implicant : IntSet.t }

type formula = {
  clauses : IntMultiMap.t;
  original_clauses : IntMultiMap.t;
  occurrence_map_1 : IntMultiMap.t;
  occurrence_map_2 : IntMultiMap.t;
  occurrence_map_n : IntMultiMap.t;
  decision_level : int;
  assignments : assignment IntMap.t;
  trail : (assignment * formula) list;
  database : IntSet.t list;
}

let of_list =
  let rec aux
      ({
         clauses;
         occurrence_map_n = omn;
         occurrence_map_2 = om2;
         occurrence_map_1 = om1;
         _;
       } as f) n = function
    | [] -> f
    | c :: cs ->
        let clauses' =
          List.fold_left (fun m l -> IntMultiMap.add m n l) clauses c
        in
        let om1', om2' =
          match List.length c with
          | 1 -> (IntMultiMap.add_all om1 n c, om2)
          | 2 -> (om1, IntMultiMap.add_all om2 n c)
          | _ -> (om1, om2)
        in
        let omn' = List.fold_left (fun m k -> IntMultiMap.add m k n) omn c in
        let f' =
          {
            f with
            clauses = clauses';
            original_clauses = clauses';
            occurrence_map_n = omn';
            occurrence_map_2 = om2';
            occurrence_map_1 = om1';
          }
        in
        aux f' (n + 1) cs
  in
  aux
    {
      clauses = IntMultiMap.empty;
      original_clauses = IntMultiMap.empty;
      occurrence_map_1 = IntMultiMap.empty;
      occurrence_map_2 = IntMultiMap.empty;
      occurrence_map_n = IntMultiMap.empty;
      decision_level = 0;
      assignments = IntMap.empty;
      trail = [];
      database = [];
    }
