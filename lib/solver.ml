open Data
open Cnf
open Problem

type result = Sat of Cnf.t | Unsat of Cnf.t

let rec cdcl max_learned_clauses max_conflicts luby f =
  let rec aux d num_learned_clauses num_conflicts f =
    let rec handle (clause, f) num_learned_clauses' =
      (* Logs.debug (fun m -> m "%s" (show f)); *)
      if decision_level f = 0 then Unsat f
      else
        let learned_clause = analyze_conflict f clause in
        (* Logs.debug (fun m -> *)
        (*     m "Learning clause %s" (Clause.show learned_clause)); *)
        match backtrack f learned_clause with
        | Ok (f', d') ->
            aux d' (num_learned_clauses' + 1) (num_conflicts + 1) f'
        | Error conflict -> handle conflict (num_learned_clauses' + 1)
    in
    (* Logs.debug (fun m -> m "%s" (show f)); *)
    if num_conflicts >= max_conflicts then
      let f = restart f in
      let max_conflicts', luby' = Luby.next luby in
      cdcl max_learned_clauses max_conflicts' luby' f
    else if num_learned_clauses >= max_learned_clauses then
      let f' = simplify f in
      cdcl (max_learned_clauses * 2) max_conflicts luby f'
    else if is_empty f then Sat f
    else
      match make_decision f with
      | Error conflict -> handle conflict num_learned_clauses
      | Ok f' -> aux (d + 1) num_learned_clauses num_conflicts f'
  in
  aux 0 0 0 f

let solve
    {
      formula = f;
      num_clauses;
      config = { base_num_conflicts; grow_factor; _ };
    } =
  let luby = Luby.create base_num_conflicts grow_factor in
  let f = preprocess f in
  cdcl ((num_clauses / 16) + 1) base_num_conflicts luby f
