open Frontend
open Solver
open Sys
open Unix
(* open Data.Common *)

exception Timeout

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
  try
    match solve p with
    | Sat -> print_endline "SAT"
    | Unsat -> print_endline "UNSAT"
  with Timeout -> print_endline "Solver timed out"
(* with _ -> *)
(*   print_endline "ERROR"; *)
(*   flush stdout *)
