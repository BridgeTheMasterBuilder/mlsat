open Frontend
open Solver

let run filename config =
  let lexbuf = Lexing.from_channel (open_in filename) in
  let p = Parser.problem Lexer.initial lexbuf in
  let p = { p with config } in
  match solve p with
  | Sat -> print_endline "SAT"
  | Unsat -> print_endline "UNSAT"
  | Timeout -> print_endline "Solver timed out"
