{
    open Parser
}

let digit=['0'-'9']
let ws=['\t' '\n' ' ']

rule initial = parse
    | "p" { P }
    | "cnf" { CNF }
    | "0" { END }
    | "%" { PERCENT }
    | digit+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | ws { initial lexbuf }
    | "c" _* { initial lexbuf }
    | eof { EOF }
