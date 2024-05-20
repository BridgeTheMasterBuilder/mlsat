open Data
open Cnf
open Problem

type result = Sat of Cnf.t | Unsat of Cnf.t

let rec cdcl decay_interval max_learned_clauses max_conflicts restart_luby
    deletion_luby f =
  let rec aux d num_learned_clauses num_conflicts f =
    let rec handle (clause, f) num_learned_clauses' =
      (* Logs.debug (fun m -> m "%s" (show f)); *)
      if decision_level f = 0 then Unsat f
      else
        let learned_clause, f' = analyze_conflict f clause in
        (* Logs.debug (fun m -> *)
        (*     m "Learning clause %s" (Clause.show learned_clause)); *)
        match backtrack f' learned_clause with
        | Ok (f'', d') ->
            aux d' (num_learned_clauses' + 1) (num_conflicts + 1) f''
        | Error conflict ->
            handle conflict (num_learned_clauses' + 1)
    in
    (* Logs.debug (fun m -> m "%s" (show f)); *)
    let f =
      if num_conflicts mod decay_interval = 0 then decay_literal_activity f
      else f
    in
    if num_conflicts >= max_conflicts then
      let f = restart f in
      let max_conflicts', restart_luby' = Luby.next restart_luby in
      cdcl decay_interval max_learned_clauses max_conflicts' restart_luby'
        deletion_luby f
    else if num_learned_clauses >= max_learned_clauses then
      let f' = forget_clauses f |> restart in
      let max_learned_clauses', deletion_luby' = Luby.next restart_luby in
      cdcl decay_interval max_learned_clauses' max_conflicts restart_luby
        deletion_luby' f'
    else
      let open Either in
      match is_empty f with
      | Right f' ->
          Sat f'
      | Left f' -> (
        match make_decision f' with
        | Error conflict ->
            handle conflict num_learned_clauses
        | Ok f' ->
            aux (d + 1) num_learned_clauses num_conflicts f' )
  in
  aux 0 0 0 f

let solve
    { formula= f
    ; config= {base_num_conflicts; grow_factor; max_learned; decay_interval; _}
    } =
  let restart_luby = Luby.create base_num_conflicts grow_factor in
  let deletion_luby = Luby.create base_num_conflicts grow_factor in
  let f = preprocess f in
  cdcl decay_interval max_learned base_num_conflicts restart_luby deletion_luby
    f
