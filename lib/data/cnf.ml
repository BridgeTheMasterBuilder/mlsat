open Common

type assignment =
  | Decision of { literal : Literal.t; level : int }
  | Implication of { literal : Literal.t; level : int; implicant : Clause.t }

type formula = {
  clauses : ClauseMap.t;
  original_clauses : ClauseMap.t;
  occur : OccurrenceMap.occurrences;
  decision_level : int;
  assignments : assignment Literal.Map.t;
  trail : (assignment * formula) list;
  database : IntSet.t list;
}

let of_list =
  let rec aux ({ clauses; occur; _ } as f) n = function
    | [] -> f
    | c :: cs ->
        let c = Clause.of_list c in
        let clauses' = ClauseMap.add c clauses in
        let occurrences = Clause.make_occurrences c n in
        let occur' = OccurrenceMap.add_occurrences occur occurrences in
        let f' =
          {
            f with
            clauses = clauses';
            original_clauses = clauses';
            occur = occur';
          }
        in
        aux f' (n + 1) cs
  in
  aux
    {
      clauses = ClauseMap.empty;
      original_clauses = ClauseMap.empty;
      occur =
        {
          occur1 = OccurrenceMap.empty;
          occur2 = OccurrenceMap.empty;
          occur_n = OccurrenceMap.empty;
        };
      decision_level = 0;
      assignments = Literal.Map.empty;
      trail = [];
      database = [];
    }
    1

let analyze_conflict { assignments = a; decision_level = d; _ } clause =
  let aux q c history = () in
  let ls = Clause.to_list clause in
  aux (CCFQueue.of_list ls) IntSet.empty
    (Literal.Set.map Literal.var (Clause.to_set clause))

let choose_literal { occur = { occur2 = o2; occur_n = om; _ }; _ } =
  let m = if OccurrenceMap.is_empty o2 then om else o2 in
  OccurrenceMap.fold
    (fun l (pos, neg) (l', m) ->
      let size_pos = IntSet.cardinal pos in
      let size_neg = IntSet.cardinal neg in
      let occurrences = size_pos + size_neg in
      let l = if size_pos < size_neg then Literal.(neg l) else l in
      if occurrences > m then (l, occurrences) else (l', m))
    m (Literal.invalid, 0)
  |> fst

let is_empty { clauses; _ } = ClauseMap.is_empty clauses

let simplify ({ clauses; occur = { occur_n = om; _ } as occur; _ } as f) l =
  match OccurrenceMap.get l om with
  | None ->
      (* f *) failwith "Attempt to simplify clause by non-existent literal"
  | Some cs ->
      ClauseMap.remove_literal_from_clauses (Literal.neg l) cs clauses
      |> Result.map (fun (clauses', os) ->
             let clauses' = ClauseMap.remove_many cs clauses' in
             let occur' = OccurrenceMap.remove l occur in
             let occur' = OccurrenceMap.update_occurrences occur' os in
             { f with clauses = clauses'; occur = occur' })

let rec unit_propagate
    ({
       assignments = a;
       original_clauses = oc;
       trail = t;
       occur = { occur1 = o1; _ };
       _;
     } as f) =
  match OccurrenceMap.choose_opt o1 with
  | Some (l, c) ->
      let ls = ClauseMap.find c oc in
      let d = 0 in
      (* TODO *)
      let i = Implication { literal = l; implicant = ls; level = d } in
      let a' = Literal.Map.add (Literal.var l) i a in
      let t' = (i, f) :: t in
      let f' = { f with assignments = a'; trail = t' } in
      simplify f' l |> Result.flat_map (fun f'' -> unit_propagate f'')
  | None -> Ok f
