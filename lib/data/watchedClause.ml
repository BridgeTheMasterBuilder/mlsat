type t = {
  clause : Literal.t Iter.iter;
  size : int;
  (* index : int; *)
  watcher1 : Literal.t;
  watcher2 : Literal.t;
}

let of_clause c =
  let open Iter in
  let clause = Clause.to_iter c |> cycle in
  match take 2 clause |> to_list with
  | [ l1; l2 ] ->
      {
        clause = drop 2 clause;
        size = Clause.size c;
        (* index = 2; *)
        watcher1 = l1;
        watcher2 = l2;
      }
  | _ -> failwith "Impossible"

let watched_literals { watcher1; watcher2; _ } = (watcher1, watcher2)

let fold f x { clause; size; _ } =
  let open Iter in
  take size clause |> fold f x

let update = Fun.id
