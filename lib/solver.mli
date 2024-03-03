open Data

type result = Sat of Literal.t list | Unsat

val solve : Problem.t -> result
