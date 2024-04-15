include CCArray

type t = Literal.t array [@@deriving show]
type clause = t

(* TODO lbd a c *)
let of_array = Fun.id
let size = length
let show c = fold (fun s l -> Printf.sprintf "%s%s " s (Literal.show l)) "" c
let to_array = Fun.id

module Watched = struct
  type t = {
    id : int;
    clause : clause;
    size : int;
    mutable index : int;
    mutable watched_literals : Literal.t * Literal.t;
  }

  type watched_clause = t

  module Set = Set.Make (struct
    type t = watched_clause

    (* TODO *)
    (* let compare { id = id1; _ } { id = id2; _ } = Int.compare id1 id2 *)
    let compare { clause = c1; _ } { clause = c2; _ } =
      Array.compare Literal.compare c1 c2
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
    let new_id = Pair.map_snd (fun x -> x + 1)
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
               (fun { id; _ } acc -> Printf.sprintf "%s%d " acc id)
               cs ""))
        m ""
  end

  type update_result =
    | WatchedLiteralChange of Map.t
    | Unit of (Literal.t * clause)
    | Falsified of clause
    | NoChange

  let fold f x { clause; _ } = clause |> Array.fold f x

  let watch_clause a c watchers =
    Array.iter
      (fun l ->
        Logs.debug (fun m ->
            m "%s(%s) " (Literal.show l)
              (Assignment.Map.find_opt (Literal.var l) a
              |> Option.map_or ~default:"_" (fun ass ->
                     Literal.show (Assignment.literal ass)))))
      c;
    let size = size c in
    to_iter c
    |> Iter.filter (fun l -> Tribool.is_nonfalse (Assignment.Map.value l a))
    |> Iter.take 2 |> Iter.to_list
    |> function
    | [ w1; w2 ] ->
        let ((_, id) as watchers') = Map.new_id watchers in
        let watched_clause =
          {
            id;
            clause = c;
            size;
            index = 2 mod size;
            watched_literals = (w1, w2);
          }
        in
        Logs.debug (fun m ->
            m "Watching clause %s (%s and %s)" (show c) (Literal.show w1)
              (Literal.show w2));
        let watchers' =
          Map.add w1 watched_clause watchers' |> Map.add w2 watched_clause
        in
        WatchedLiteralChange watchers'
    | [ w ] ->
        if Tribool.is_unknown (Assignment.Map.value w a) then Unit (w, c)
        else (
          Logs.debug (fun m -> m "Clause %s seems to be satisfied" (show c));

          NoChange)
    | _ -> Falsified c

  let unwatch_clause c watchers =
    (* Clause.to_iter clause *)
    (* |> fold *)
    (*      (fun watchers' l -> Clause.Watched.Map.remove l n watchers') *)
    (*      watchers *)
    watchers

  let update l a ({ clause; size; index; watched_literals = w1, w2; _ } as c)
      watchers =
    let other_watched_literal = if Literal.equal l w1 then w2 else w1 in
    let other_watched_literal_truth_value =
      Assignment.Map.value other_watched_literal a
    in
    if Tribool.is_true other_watched_literal_truth_value then NoChange
    else
      let result =
        let open Iter in
        let data = clause in
        0 -- (size - 1)
        |> find_map (fun i ->
               let index' = (index + i) mod size in
               let l' = Array.unsafe_get data index' in
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
          c.index <- index';
          c.watched_literals <- (other_watched_literal, new_watched_literal);
          let watchers' =
            Map.remove l c watchers |> Map.add new_watched_literal c
          in
          WatchedLiteralChange watchers'
end

module Set = struct
  include Iter.Set.Make (struct
    type t = clause

    let compare c1 c2 = Array.compare Literal.compare c1 c2
  end)

  let show s = fold (fun c s -> Printf.sprintf "%s:%s\n" s (show c)) s ""
end
