open Data
open Cnf
open Problem

type result = Sat of formula | Unsat of formula

(* TODO
   when first constructing the formula, immediately unit propagate everything possible

   then instead of calling unit propagate here, just make a decision (otherwise it would be SAT or UNSAT already)
   and unit propagate immediately everything until a conflict is reached, etc.
*)
let rec cdcl max_conflicts luby f =
  let rec aux d max_conflicts' conflicts f =
    let rec handle (clause, f) =
      if decision_level f = 0 then Unsat f
      else
        let learned_clause = analyze_conflict f clause in
        match backtrack f learned_clause with
        | Ok (f', d') -> aux d' max_conflicts' (conflicts + 1) f'
        | Error conflict -> handle conflict
    in
    if conflicts = max_conflicts' then
      let f = restart f in
      let max_conflicts', luby' = Luby.next luby in
      cdcl max_conflicts' luby' f
    else if is_empty f then Sat f
    else
      match make_decision f with
      | Error conflict -> handle conflict
      | Ok f' -> aux (d + 1) max_conflicts' conflicts f'
  in
  aux 0 max_conflicts 0 f

let solve { formula = f; config = { base_num_conflicts; grow_factor; _ } } =
  let luby = Luby.create base_num_conflicts grow_factor in
  let f = preprocess f in
  cdcl base_num_conflicts luby f
