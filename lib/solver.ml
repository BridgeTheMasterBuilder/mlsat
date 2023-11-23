open Data
open Cnf

type result = Sat | Unsat | Timeout

let rec cdcl max_conflicts grow f =
  let rec recur d max_conflicts' conflicts f =
    if conflicts = max_conflicts' then
      let f' = restart f in
      cdcl (max_conflicts' * grow) grow f'
    else
      match unit_propagate f with
      | Error (clause, f') ->
          if d = 0 then Unsat
          else
            let learned_clause = analyze_conflict f' clause in
            let f'', d' = backtrack f' learned_clause in
            recur d' max_conflicts' (conflicts + 1)
              (add_clause f'' learned_clause learned_clause)
      | Ok f' ->
          if null f' then Sat
          else
            let l = choose_literal f' in
            let l_true = rewrite f' l in
            recur (d + 1) max_conflicts' conflicts l_true
  in
  recur 0 max_conflicts 0 f

let solve
    ({ formula = f; config = { time_limit; base_num_conflicts; grow_factor } } :
      Problem.t) =
  match unit_propagate f with
  | Error _ -> Unsat
  | Ok f -> cdcl base_num_conflicts grow_factor f
