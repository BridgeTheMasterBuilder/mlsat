include Queue

(* TODO API *)
type t = (Literal.t * Clause.t) Queue.t

let history = ref Literal.Set.empty

let clear _ =
  history := Literal.Set.empty;
  create ()

let empty : t = create ()

let show (m : t) =
  fold (fun acc (_, c) -> Printf.sprintf "%s( %s)\n" acc (Clause.show c)) "" m

let snoc q ((l, c) as x) =
  if Literal.Set.mem l !history then q
  else (
    history := Literal.Set.add l !history;
    add x q;
    q)

let take_front q = take_opt q |> Option.map (fun x -> (x, q))
