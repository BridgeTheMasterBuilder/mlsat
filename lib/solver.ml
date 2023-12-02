open Data
open Cnf
open Problem

type result = Sat | Unsat

let rec cdcl max_conflicts grow f =
  let rec aux d max_conflicts' conflicts f =
    if conflicts = max_conflicts' then
      let f = restart f in
      cdcl (max_conflicts' * grow) grow f
    else
      match unit_propagate f with
      | Error (clause, f) ->
          if d = 0 then Unsat
          else
            let learned_clause = analyze_conflict f clause in
            let f', d' = backtrack f learned_clause in
            let f' = add_clause f' learned_clause in
            aux d' max_conflicts' (conflicts + 1) f'
      | Ok f ->
          if is_empty f then Sat
          else
            let l = choose_literal f in
            let f' = rewrite f l in
            aux (d + 1) max_conflicts' conflicts f'
  in
  aux 0 max_conflicts 0 f

let solve { formula = f; config = { base_num_conflicts; grow_factor; _ } } =
  match unit_propagate f with
  | Error _ -> Unsat
  | Ok f -> cdcl base_num_conflicts grow_factor f
