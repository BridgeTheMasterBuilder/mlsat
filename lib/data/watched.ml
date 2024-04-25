module L = Literal

type watched_clause = {
  clause : Clause.t;
  size : int;
  mutable index : int;
  mutable watched_literals : L.t * L.t;
}

module ClauseSet = struct
  include CCHashSet.Make (struct
    type t = watched_clause

    let equal { clause = c1; _ } { clause = c2; _ } = Clause.equal c1 c2
    let hash { clause; _ } = Clause.hash clause
  end)

  let add c s =
    insert s c;
    s

  let remove c s =
    remove s c;
    s
end

module Literal = struct
  module Map = struct
    module M = CCHashtbl.Make (L)
    include M

    type t = ClauseSet.t M.t
    type key = L.t

    let add l n m =
      update m ~k:l ~f:(fun _ -> function
        | Some s -> Some (ClauseSet.add n s)
        | None -> Some (ClauseSet.singleton n));
      m

    let make n = create n
    let find_opt l m = find_opt m l
    let is_empty m = length m = 0

    let remove l c m =
      update m ~k:l ~f:(fun _ -> function
        | Some s -> Some (ClauseSet.remove c s) (* TODO *) | None -> None);
      m

    let show m =
      fold
        (fun l cs s ->
          Printf.sprintf "%s%s:%s\n" s (L.show l)
            (ClauseSet.fold
               (fun acc { clause; _ } ->
                 Printf.sprintf "%s( %s) " acc (Clause.show clause))
               "" cs))
        m ""
  end
end

module Clause = struct
  type t = watched_clause

  module Set = ClauseSet

  type update_result =
    | WatchedLiteralChange of Literal.Map.t
    | Unit of (L.t * Clause.t)
    | Falsified of Clause.t
    | NoChange

  let to_clause { clause; _ } = clause

  let unwatch _c watchers =
    (* Clause.to_iter clause *)
    (* |> fold *)
    (*      (fun watchers' l -> Clause.Watched.Map.remove l n watchers') *)
    (*      watchers *)
    watchers

  let update l a
      ({ clause; size; index; watched_literals = w1, w2; _ } as watched_clause)
      watchers =
    let other_watched_literal = if L.equal l w1 then w2 else w1 in
    let other_watched_literal_truth_value =
      Assignment.Map.value other_watched_literal a
    in
    if Tribool.is_true other_watched_literal_truth_value then NoChange
    else
      let result =
        let open Iter in
        0 -- (size - 1)
        |> find_map (fun i ->
               let index' = (index + i) mod size in
               let c = Clause.to_array clause in
               let l' = Array.unsafe_get c index' in
               if
                 Tribool.is_false (Assignment.Map.value l' a)
                 || L.equal l' other_watched_literal
               then None
               else Some (index', l'))
      in
      match result with
      | None ->
          if Tribool.is_false other_watched_literal_truth_value then
            Falsified clause
          else Unit (other_watched_literal, clause)
      | Some (index', new_watched_literal) ->
          watched_clause.index <- index';
          watched_clause.watched_literals <-
            (other_watched_literal, new_watched_literal);
          let watchers' =
            Literal.Map.remove l watched_clause watchers
            |> Literal.Map.add new_watched_literal watched_clause
          in
          WatchedLiteralChange watchers'

  let watch a clause watchers =
    let size = Clause.size clause in
    (* TODO is this better? *)
    Clause.to_iter clause
    |> Iter.map (fun l -> (l, Assignment.Map.value l a))
    |> Iter.filter (fun (_, v) -> Tribool.is_nonfalse v)
    |> Iter.take 2 |> Iter.to_list
    |> function
    | [ (w1, _); (w2, _) ] ->
        let watched_clause =
          { clause; size; index = 2 mod size; watched_literals = (w1, w2) }
        in
        let watchers' =
          Literal.Map.add w1 watched_clause watchers
          |> Literal.Map.add w2 watched_clause
        in
        WatchedLiteralChange watchers'
    | [ (w, v) ] -> if Tribool.is_unknown v then Unit (w, clause) else NoChange
    | _ -> Falsified clause
end
