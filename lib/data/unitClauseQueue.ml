(* include Queue *)

module X = struct
  type t = Literal.t * Clause.t

  let compare (l1, _) (l2, _) = Literal.compare l1 l2
end

(* TODO API *)
module M = Workqueue.Make (X)
include M

type key = elt
(* type t = key Queue.t *)

(* let clear _ = create () *)
let clear _ = empty
(* let empty = create () *)

let snoc q x = push x q

(* let take_front q = take_opt q |> Option.map (fun x -> (x, q)) *)
let take_front = pop
