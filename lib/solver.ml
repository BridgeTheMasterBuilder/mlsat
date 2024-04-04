open Data
open Cnf
open Problem

type result = Sat of formula | Unsat of formula

let rec cdcl max_conflicts luby f =
  let rec aux d max_conflicts' conflicts f =
    let handle (clause, f) =
      if d = 0 then Unsat f
      else
        let learned_clause = analyze_conflict f clause in
        let f', d' = backtrack f learned_clause in
        aux d' max_conflicts' (conflicts + 1) f'
    in
    (* check_invariants f; *)
    (* Logs.debug (fun m -> m "%s" (show f)); *)
    if conflicts = max_conflicts' then
      let f = restart f in
      let max_conflicts', luby' = Luby.next luby in
      cdcl max_conflicts' luby' f
    else
      match unit_propagate f with
      | Error conflict -> handle conflict
      | Ok f -> (
          if is_empty f then Sat f
          else
            match make_decision f with
            | Error conflict -> handle conflict
            | Ok f' -> aux (d + 1) max_conflicts' conflicts f')
  in
  aux 0 max_conflicts 0 f

let solve { formula = f; config = { base_num_conflicts; grow_factor; _ } } =
  (* Logs.debug (fun m -> m "%s" (show f)); *)
  match unit_propagate f with
  | Error (_, f) -> Unsat f
  | Ok f ->
      if is_empty f then Sat f
      else
        let luby = Luby.create base_num_conflicts grow_factor in
        let f = preprocess f in
        cdcl base_num_conflicts luby f
