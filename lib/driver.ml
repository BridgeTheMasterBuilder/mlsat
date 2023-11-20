open Frontend
open Solver

let run filename config =
  let lexbuf = Lexing.from_channel (open_in filename) in
  let p =
    try Parser.problem Lexer.initial lexbuf
    with _ ->
      failwith
        (string_of_int (Lexing.lexeme_start lexbuf)
        ^ ": " ^ Lexing.lexeme lexbuf)
  in
  let p = { p with config } in
  try
    match solve p with
    | Sat -> print_endline "SAT"
    | Unsat -> print_endline "UNSAT"
    | Timeout -> print_endline "Solver timed out"
  with _ ->
    print_endline "ERROR";
    flush stdout
