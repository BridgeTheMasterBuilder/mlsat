open Data
open Cnf

type result = Sat | Unsat | Timeout

let restart f = f

let rec cdcl max_conflicts grow =
  let rec aux d max_conflicts conflicts f =
    print_endline "Checking invariant in recursive call";
    check_invariants f;
    if conflicts = max_conflicts then (
      let f' = restart f in
      print_endline "Checking invariant after restart";
      check_invariants f';
      cdcl (max_conflicts * grow) grow f')
    else
      match unit_propagate f with
      | Error clause ->
          if d = 0 then Unsat
          else
            let learned_clause = analyze_conflict f clause in
            print_endline ("Learning clause: " ^ Clause.show learned_clause);
            let f', d' = backtrack f learned_clause in
            print_endline "Checking invariant after backtracking";
            check_invariants f';
            aux d' max_conflicts (conflicts + 1)
              (add_clause f' learned_clause learned_clause)
      | Ok f' ->
          print_endline "Checking invariant after successful unit propagation";
          check_invariants f';
          if is_empty f' then Sat
          else
            let l = choose_literal f' in
            print_endline ("making decision: " ^ Literal.show l);
            let f'' = rewrite f' l in
            print_endline "Checking invariant after rewriting";
            check_invariants f'';
            aux (d + 1) max_conflicts conflicts f''
  in
  fun f ->
    print_endline (show f);
    aux 0 max_conflicts 0 f

let solve
    ({ formula = f; config = { time_limit; base_num_conflicts; grow_factor } } :
      Problem.t) =
  match unit_propagate f with
  | Error _ -> Unsat
  | Ok f -> cdcl base_num_conflicts grow_factor f
