module M = struct
  type t = {
    id : int; (* TODO hack? *)
    clause : Literal.t array;
    size : int;
    index : int;
    watchers : (Literal.t * Literal.t) option;
  }

  let compare { clause = c1; id = id1; _ } { clause = c2; id = id2; _ } =
    compare id1 id2
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

let of_clause c id =
  (* TODO assert clause is not empty? *)
  let open Iter in
  let clause = Clause.to_iter c |> to_array in
  let size = Array.length clause in
  let watchers = if size >= 2 then Some (clause.(0), clause.(1)) else None in
  { id; clause; size; index = 2 mod size; watchers }

type update_result =
  | WatcherChange of (Literal.t * Literal.t * Literal.t * t)
  | Unit of t
  | Falsified of t
  | NoChange

(* let update l a ({ clause; size; index; watchers; _ } as c) = *)
(*   let open Iter in *)
(*   let w1, w2 = *)
(*     let w1, w2 = Option.get_exn_or "UPDATE" watchers in *)
(*     if Literal.equal w1 l then (w1, w2) else (w2, w1) *)
(*   in *)
(*   let other_watcher_needs_updating = *)
(*     match Assignment.Map.find_opt w2 a with *)
(*     | Some ass *)
(*       when let l' = Assignment.literal ass in *)
(*            Literal.signum w2 <> Literal.signum l' -> *)
(*         true *)
(*     | _ -> false *)
(*   in *)
(*   if not other_watcher_needs_updating then NoChange *)
(*   else ( *)
(*     Logs.debug (fun m -> *)
(*         m "%d (watching %s and %s - index %d): " c.id (Literal.show w1) *)
(*           (Literal.show w2) index); *)
(*     Array.iter *)
(*       (fun l -> *)
(*         Logs.debug (fun m -> *)
(*             m "%s(%s) " (Literal.show l) *)
(*               (Assignment.Map.find_opt l a *)
(*               |> Option.map_or ~default:"_" (fun ass -> *)
(*                      Literal.show (Assignment.literal ass))))) *)
(*       c.clause; *)
(*     let { index = index'; watcher1_change; watcher2_change; clause_falsified } = *)
(*       0 -- (size - 1) *)
(*       |> fold_while *)
(*            (fun { *)
(*                   index = index'; *)
(*                   watcher1_change; *)
(*                   watcher2_change; *)
(*                   clause_falsified = falsified'; *)
(*                   _; *)
(*                 } _ -> *)
(*              let index'' = (index' + 1) mod size in *)
(*              let l' = clause.(index') in *)
(*              match Assignment.Map.find_opt l' a with *)
(*              | Some ass *)
(*                when let l'' = Assignment.literal ass in *)
(*                     Literal.signum l' <> Literal.signum l'' -> *)
(*                  ( { *)
(*                      index = index''; *)
(*                      watcher1_change; *)
(*                      watcher2_change; *)
(*                      clause_falsified = falsified'; *)
(*                    }, *)
(*                    `Continue ) *)
(*              | _ -> ( *)
(*                  match watcher1_change with *)
(*                  | None -> *)
(*                      ( { *)
(*                          index = index''; *)
(*                          watcher1_change = *)
(*                            (if Literal.equal w2 l' then None else Some l'); *)
(*                          watcher2_change = None; *)
(*                          clause_falsified = false; *)
(*                        }, *)
(*                        if other_watcher_needs_updating then `Continue else `Stop *)
(*                      ) *)
(*                  | Some l'' -> *)
(*                      ( { *)
(*                          index = index''; *)
(*                          watcher1_change; *)
(*                          watcher2_change = *)
(*                            (if Literal.equal l' l'' then None else Some l'); *)
(*                          clause_falsified = false; *)
(*                        }, *)
(*                        `Stop ))) *)
(*            { *)
(*              index; *)
(*              watcher1_change = None; *)
(*              watcher2_change = None; *)
(*              clause_falsified = true; *)
(*            } *)
(*     in *)
(*     if clause_falsified then Falsified c *)
(*     else *)
(*       match (watcher1_change, watcher2_change) with *)
(*       | Some w1', Some w2' -> *)
(*           let c' = { c with watchers = Some (w1', w2'); index = index' } in *)
(*           WatcherChange (w1, w1', w2, w2', c') *)
(*       | Some w1', None when not other_watcher_needs_updating -> *)
(*           let c' = { c with watchers = Some (w1', w2); index = index' } in *)
(*           WatcherChange (w1, w1', w2, w2, c') *)
(*       | _ -> Unit c) *)

let update l a ({ clause; size; index; watchers; _ } as c) =
  let open CCEither in
  let open Iter in
  let w1, w2 = Option.get_exn_or "UPDATE" watchers in
  let other_watcher = if Literal.equal l w1 then Right w2 else Left w1 in
  let uneither = function Left w | Right w -> w in
  if Assignment.is_true (uneither other_watcher) a then NoChange
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
      0 -- (size - 1)
      |> find_map (fun i ->
             let index' = (index + i) mod size in
             let l' = clause.(index) in
             if
               Assignment.is_false l' a
               || Literal.equal l' (uneither other_watcher)
             then None
             else Some (index', l'))
    in
    match result with
    | None ->
        if Assignment.is_false (uneither other_watcher) a then Falsified c
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
        WatcherChange (l, new_watcher, uneither other_watcher, c'))

let watched_literals { watchers; _ } = watchers
