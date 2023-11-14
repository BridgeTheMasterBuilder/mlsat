open Data
open Cnf

type result = Sat | Unsat | Timeout

let restart f = f

let rec cdcl max_conflicts grow =
  let rec aux d max_conflicts conflicts f =
    if conflicts = max_conflicts then
      let f' = restart f in
      cdcl (max_conflicts * grow) grow f'
    else
      match unit_propagate f with
      | Error clause ->
          if d = 0 then Unsat
          else
            let learned_clause = analyze_conflict f clause in
            let f', d' = backtrack f learned_clause in
            aux d' max_conflicts (conflicts + 1)
              (add_clause f' learned_clause learned_clause)
      | Ok f' ->
          if is_empty f' then Sat
          else
            let l = choose_literal f' in
            let f'' = rewrite f' l in
            aux (d + 1) max_conflicts conflicts f''
  in
  aux 0 max_conflicts 0

let solve
    ({ formula = f; time_limit; base_num_conflicts; grow_factor } : Problem.t) =
  match unit_propagate f with
  | Error _ -> Unsat
  | Ok f -> cdcl base_num_conflicts grow_factor f
