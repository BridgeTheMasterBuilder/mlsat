open Frontend
open Solver
open Sys
open Unix
open Data
open Cnf

exception Timeout

let emit_proof_of_unsatisfiability clauses =
  let out_filename = "proof.out" in
  let oc = open_out out_filename in
  List.iter (fun c -> Printf.fprintf oc "%s0\n" (Clause.show c)) clauses;
  Printf.fprintf oc "0"

let run filename config =
  let lexbuf = Lexing.from_channel (open_in filename) in
  let p =
    (* try Parser.problem Lexer.initial lexbuf *)
    Parser.problem Lexer.initial lexbuf
  in
  let p = { p with config } in
  set_signal sigalrm (Signal_handle (fun _ -> raise_notrace Timeout));
  setitimer ITIMER_REAL { it_value = config.time_limit; it_interval = 0.0 }
  |> ignore;
  if config.verbose then (
    Logs.set_reporter (Logs_fmt.reporter ());
    Logs.set_level (Some Debug))
  else (
    Logs.set_reporter (Logs_fmt.reporter ());
    Logs.set_level (Some Error));
  try
    match solve p with
    | Sat f ->
        if not (verify_sat f) then Logs.err (fun m -> m "%s" (show f));
        assert (verify_sat f);
        print_endline "s SATISFIABLE"
    | Unsat f ->
        emit_proof_of_unsatisfiability (learned_clauses f);
        print_endline "s UNSATISFIABLE"
  with Timeout -> print_endline "s UNKNOWN"
(* with _ -> *)
(*   print_endline "ERROR"; *)
(*   flush stdout *)
