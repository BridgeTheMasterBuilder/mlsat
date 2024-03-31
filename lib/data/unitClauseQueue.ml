include Queue

type t = (int * Clause.t) Queue.t

let empty : t = create ()

let snoc q x =
  add x q;
  q

let take_front q = take_opt q |> Option.map (fun x -> (x, q))
