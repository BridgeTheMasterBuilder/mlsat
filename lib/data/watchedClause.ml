module M = struct
  type t = {
    id : int; (* TODO hack? *)
    clause : Literal.t array;
    size : int;
    index : int;
    watchers : (Literal.t * Literal.t) option;
  }

  let compare { id = id1; _ } { id = id2; _ } = compare id1 id2
end

type t = M.t

open M
module Set = Set.Make (M)

module Map = struct
  include Literal.Map

  type t = Set.t Literal.Map.t
  type key = Literal.t

  let add l n =
    update l (function
      | Some s -> Some (Set.add n s)
      | None -> Some (Set.singleton n))

  let remove l n =
    update l (function
      | Some s -> Some (Set.remove n s)
      | None -> failwith "REMOVE")

  let show o =
    fold
      (fun l cs s ->
        Printf.sprintf "%s%s:%s\n" s (Literal.show l)
          (Set.fold (fun { id; _ } acc -> Printf.sprintf "%s%d " acc id) cs ""))
      o ""
end

let fold f x { clause; _ } = Array.fold f x clause

let of_clause a c id =
  let open Iter in
  let clause_iter = Clause.to_iter c in
  let clause = clause_iter |> to_array in
  let size = Array.length clause in
  let watchers =
    filter (fun l -> Tribool.is_nonfalse (Assignment.Map.value l a)) clause_iter
    |> take 2 |> to_list
    |> function
    | [ w1; w2 ] -> Some (w1, w2)
    | _ -> None
  in
  { id; clause; size; index = 2 mod size; watchers }

type update_result =
  | WatcherChange of (Literal.t * Literal.t * Literal.t * t)
  | Unit of t
  | Falsified of t
  | NoChange

let update l a ({ clause; size; index; watchers; _ } as c) =
  let open CCEither in
  let open Iter in
  let w1, w2 = Option.get_exn_or "UPDATE" watchers in
  let other_watcher, other_watcher_literal =
    if Literal.equal l w1 then (Right w2, w2) else (Left w1, w1)
  in
  let other_watcher_truth_value =
    Assignment.Map.value other_watcher_literal a
  in
  if Tribool.is_true other_watcher_truth_value then (
    Logs.debug (fun m ->
        m "Clause is satisfied by %s, doing nothing"
          (Literal.show other_watcher_literal));
    NoChange)
  else (
    Logs.debug (fun m ->
        m "%d (watching %s and %s - index %d): " c.id (Literal.show w1)
          (Literal.show w2) index);
    Array.iter
      (fun l ->
        Logs.debug (fun m ->
            m "%s(%s) " (Literal.show l)
              (Assignment.Map.find_opt l a
              |> Option.map_or ~default:"_" (fun ass ->
                     Literal.show (Assignment.literal ass)))))
      c.clause;
    let result =
      Logs.debug (fun m -> m "Trying to find a new watcher");
      0 -- (size - 1)
      |> find_map (fun i ->
             let index' = (index + i) mod size in
             let l' = clause.(index') in
             if
               Tribool.is_false (Assignment.Map.value l' a)
               || Literal.equal l' other_watcher_literal
             then (
               Logs.debug (fun m ->
                   m "No good, %s is either false or already watched"
                     (Literal.show l'));
               None)
             else (
               Logs.debug (fun m -> m "Found new watcher %s" (Literal.show l'));
               Some (index', l')))
    in
    match result with
    | None ->
        if Tribool.is_false other_watcher_truth_value then Falsified c
        else Unit c
    | Some (index', new_watcher) ->
        let c' =
          {
            c with
            index = index';
            watchers =
              (match other_watcher with
              | Left w -> Some (w, new_watcher)
              | Right w -> Some (new_watcher, w));
          }
        in
        WatcherChange (l, new_watcher, other_watcher_literal, c'))

let watched_literals { watchers; _ } = watchers
