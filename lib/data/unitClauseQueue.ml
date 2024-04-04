include Queue

(* TODO API *)
type key = Literal.t * Clause.t
type t = key Queue.t

let clear _ = create ()
let empty = create ()

let snoc q x =
  add x q;
  q

let take_front q = take_opt q |> Option.map (fun x -> (x, q))
