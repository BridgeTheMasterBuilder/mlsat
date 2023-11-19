{
    open Parser
}

let literal= '-'? ['1'-'9']+
let ws=['\t' '\n' ' ']

rule initial = parse
    | "p" { P }
    | "cnf" { CNF }
    | "0" { END }
    | "%" { PERCENT }
    | literal { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | ws { initial lexbuf }
    | "c" ['\n'] { initial lexbuf }
    | "c" [^'n' '\n'] [^'\n']* ['\n'] { initial lexbuf }
    | _ { failwith (Lexing.lexeme lexbuf) }
    | eof { EOF }
