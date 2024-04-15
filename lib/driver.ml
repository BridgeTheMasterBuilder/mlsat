open Frontend
open Solver
open Sys
open Unix
open Data
open Cnf
open Config

exception Timeout

let emit_proof_of_unsatisfiability filename clauses =
  let out_filename = Filename.remove_extension filename ^ ".out" in
  let oc = open_out out_filename in
  List.iter
    (fun c -> Printf.fprintf oc "%s0\n" (Clause.show c))
    (List.rev clauses);
  Printf.fprintf oc "0"

let run filename config =
  let lexbuf = Lexing.from_channel (open_in filename) in
  match Parser.problem Lexer.initial lexbuf with
  | None ->
      Option.iter
        (fun filename -> emit_proof_of_unsatisfiability filename [])
        config.emit_proof;
      Printf.printf "s UNSATISFIABLE\nc Learned 0 clauses\n"
  | Some p -> (
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
            check f;
            Printf.printf "s SATISFIABLE\nv ";
            List.iter
              (fun l -> Printf.printf "%s " (Literal.show l))
              (assignments f);
            print_endline "0"
        | Unsat f ->
            check f;
            Option.iter
              (fun filename ->
                emit_proof_of_unsatisfiability filename (learned_clauses f))
              config.emit_proof;
            Printf.printf "s UNSATISFIABLE\nc Learned %d clauses\n"
              (List.length (learned_clauses f))
      with Timeout -> print_endline "s UNKNOWN")
