open Frontend

let run filename =
  let lexbuf = Lexing.from_channel (open_in filename) in
  let p = Parser.problem Lexer.initial lexbuf in
  ()
