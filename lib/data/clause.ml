include CCArray

type t = CCHash.hash * Literal.t array
type clause = t

let of_list l =
  let a = of_list l in
  (CCHash.array Literal.hash a, a)

let size (_, c) = length c

let show (_, c) =
  fold (fun s l -> Printf.sprintf "%s%s " s (Literal.show l)) "" c

let to_array (_, c) = c
let to_iter (_, c) = to_iter c

module Watched = struct
  type t = {
    clause : clause;
    size : int;
    mutable index : int;
    mutable watched_literals : Literal.t * Literal.t;
  }

  type watched_clause = t

  module Set = Set.Make (struct
    type t = watched_clause

    let compare { clause = id1, _; _ } { clause = id2, _; _ } =
      Int.compare id1 id2
  end)

  module Map = struct
    module M = CCHashtbl.Make (Literal)
    include M

    type t = Set.t M.t * int
    type key = Literal.t

    let add l n =
      Pair.map_fst (fun m ->
          update m ~k:l ~f:(fun _ -> function
            | Some s -> Some (Set.add n s) | None -> Some (Set.singleton n));
          m)

    let make n = (create n, 0)
    let find_opt l (m, _) = find_opt m l
    let is_empty (m, _) = length m = 0

    let remove l c =
      Pair.map_fst (fun m ->
          update m ~k:l ~f:(fun _ -> function
            | Some s -> Some (Set.remove c s) (* TODO *) | None -> None);
          m)

    let show (m, _) =
      fold
        (fun l cs s ->
          Printf.sprintf "%s%s:%s\n" s (Literal.show l)
            (Set.fold
               (fun { clause; _ } acc ->
                 Printf.sprintf "%s( %s) " acc (show clause))
               cs ""))
        m ""
  end

  type update_result =
    | WatchedLiteralChange of Map.t
    | Unit of (Literal.t * clause)
    | Falsified of clause
    | NoChange

  let to_clause { clause; _ } = clause

  let unwatch_clause c watchers =
    (* Clause.to_iter clause *)
    (* |> fold *)
    (*      (fun watchers' l -> Clause.Watched.Map.remove l n watchers') *)
    (*      watchers *)
    watchers

  let update l a
      ({ clause = (_, c) as clause; size; index; watched_literals = w1, w2; _ }
      as watched_clause) watchers =
    let other_watched_literal = if Literal.equal l w1 then w2 else w1 in
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
               let l' = Array.unsafe_get c index' in
               if
                 Tribool.is_false (Assignment.Map.value l' a)
                 || Literal.equal l' other_watched_literal
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
            Map.remove l watched_clause watchers
            |> Map.add new_watched_literal watched_clause
          in
          WatchedLiteralChange watchers'

  let watch_clause a ((_, c) as clause) watchers =
    Array.iter
      (fun l ->
        Logs.debug (fun m ->
            m "%s(%s) " (Literal.show l)
              (Assignment.Map.find_opt (Literal.var l) a
              |> Option.map_or ~default:"_" (fun ass ->
                     Literal.show (Assignment.literal ass)))))
      c;
    let size = size clause in
    (* TODO is this better? *)
    to_iter clause
    |> Iter.map (fun l -> (l, Assignment.Map.value l a))
    |> Iter.filter (fun (_, v) -> Tribool.is_nonfalse v)
    |> Iter.take 2 |> Iter.to_list
    |> function
    | [ (w1, _); (w2, _) ] ->
        let watched_clause =
          { clause; size; index = 2 mod size; watched_literals = (w1, w2) }
        in
        Logs.debug (fun m ->
            m "Watching clause %s (%s and %s)" (show clause) (Literal.show w1)
              (Literal.show w2));
        let watchers' =
          Map.add w1 watched_clause watchers |> Map.add w2 watched_clause
        in
        WatchedLiteralChange watchers'
    | [ (w, v) ] ->
        if Tribool.is_unknown v then Unit (w, clause)
        else (
          Logs.debug (fun m ->
              m "Clause %s seems to be satisfied" (show clause));

          NoChange)
    | _ -> Falsified clause
end

module Set = struct
  include Iter.Set.Make (struct
    type t = clause

    let compare (id1, _) (id2, _) = Int.compare id1 id2
  end)

  let show s = fold (fun c s -> Printf.sprintf "%s:%s\n" s (show c)) s ""
end
