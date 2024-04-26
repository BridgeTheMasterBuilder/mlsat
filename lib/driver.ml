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
  Vector.iter
    (fun c ->
      let open Database in
      match c with
      | Addition c ->
          Printf.fprintf oc "%s0\n" (Clause.show c)
      | Deletion c ->
          Printf.fprintf oc "d %s0\n" (Clause.show c) )
    clauses ;
  Printf.fprintf oc "0"

let run filename config =
  let lexbuf = Lexing.from_channel (open_in filename) in
  let v, c, clause_list = Parser.problem Lexer.initial lexbuf in
  match Cnf.of_list v c config clause_list with
  | None ->
      Option.iter
        (fun filename ->
          emit_proof_of_unsatisfiability filename (Vector.create ()) )
        config.emit_proof ;
      Printf.printf "s UNSATISFIABLE\nc Learned 0 clauses\n"
  | Some formula -> (
      let open Problem in
      let p = {formula; config} in
      set_signal sigalrm (Signal_handle (fun _ -> raise_notrace Timeout)) ;
      setitimer ITIMER_REAL {it_value= config.time_limit; it_interval= 0.0}
      |> ignore ;
      if config.verbose then (
        Logs.set_reporter (Logs_fmt.reporter ()) ;
        Logs.set_level (Some Debug) )
      else (
        Logs.set_reporter (Logs_fmt.reporter ()) ;
        Logs.set_level (Some Error) ) ;
      try
        match solve p with
        | Sat f ->
            (* check f; *)
            Printf.printf "s SATISFIABLE\nv " ;
            List.iter
              (fun l -> Printf.printf "%s " (Literal.show l))
              (assignments f) ;
            print_endline "0"
        | Unsat f ->
            let trace = trace f in
            (* check f; *)
            Option.iter
              (fun filename -> emit_proof_of_unsatisfiability filename trace)
              config.emit_proof ;
            Printf.printf "s UNSATISFIABLE\nc Learned %d clauses\n"
              (Vector.length trace)
      with Timeout -> print_endline "s UNKNOWN" )
