open Data
open Cnf
open Problem

type result = Sat | Unsat

let rec cdcl max_conflicts luby f =
  let rec aux d max_conflicts' conflicts f =
    (* check_invariants f; *)
    print_endline (show f);
    if conflicts = max_conflicts' then
      let f = restart f in
      let max_conflicts', luby' = Luby.next luby in
      cdcl max_conflicts' luby' f
    else
      (* print_endline "Starting unit prop"; *)
      match unit_propagate f with
      | Error (clause, f) ->
          if d = 0 then Unsat
          else
            let learned_clause = analyze_conflict f clause in
            (* Printf.printf "Learned clause: %s\n" (Clause.show learned_clause); *)
            let f', d' = backtrack f learned_clause in
            let f' = add_clause f' learned_clause learned_clause in
            (* print_endline (show f); *)
            aux d' max_conflicts' (conflicts + 1) f'
      | Ok f ->
          if is_empty f then Sat
          else
            (* let l = choose_literal f in *)
            (* let f' = rewrite f l in *)
            let f' = make_decision f in
            aux (d + 1) max_conflicts' conflicts f'
  in
  aux 0 max_conflicts 0 f

let solve { formula = f; config = { base_num_conflicts; grow_factor; _ } } =
  match unit_propagate f with
  | Error _ -> Unsat
  | Ok f ->
      if is_empty f then Sat
      else
        let luby = Luby.create base_num_conflicts grow_factor in
        let f = preprocess f in
        cdcl base_num_conflicts luby f
