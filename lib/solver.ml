open Data
open Cnf
open Problem

type result = Sat | Unsat

module Luby = struct
  type t = { unit_run : int; phase : int; grow : int; max : int }

  let create unit_run grow = { unit_run; phase = 0; grow; max = grow }

  let next ({ unit_run; phase; grow; max } as l) =
    match phase with
    | 0 | 1 -> (unit_run, { l with phase = phase + 1 })
    | step ->
        if step = max then
          (unit_run * step, { l with phase = 0; max = max * grow })
        else (unit_run * step, { l with phase = step * grow })
end

let rec cdcl max_conflicts luby f =
  let rec aux d max_conflicts' conflicts f =
    if conflicts = max_conflicts' then
      let f = restart f in
      let max_conflicts', luby' = Luby.next luby in
      (* print_endline ("Next Luby number: " ^ string_of_int max_conflicts'); *)
      cdcl max_conflicts' luby' f
    else
      match unit_propagate f with
      | Error (clause, f) ->
          if d = 0 then Unsat
          else
            let learned_clause = analyze_conflict f clause in
            let f', d' = backtrack f learned_clause in
            let f' = add_clause f' learned_clause learned_clause in
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
  | Ok f ->
      let luby = Luby.create base_num_conflicts grow_factor in
      cdcl base_num_conflicts luby f
