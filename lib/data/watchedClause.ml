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
  (* Array.compare (fun l1 l2 -> Literal.compare l1 l2) c1 c2 *)
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
    (* Printf.printf "Removing %d from %s's watchlist\n" n.id (Literal.show l); *)
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
  assert (size > 0);
  let watchers = if size >= 2 then Some (clause.(0), clause.(1)) else None in
  { id; clause; size; index = 2 mod size; watchers }

type update_result =
  | WatcherChange of (Literal.t * Literal.t * Literal.t * Literal.t * t)
  | Unit of t
  | Falsified of t

type foo = {
  index : int;
  watcher1_change : Literal.t option;
  watcher2_change : Literal.t option;
  clause_falsified : bool;
}

let update l a ({ clause; size; index; watchers; _ } as c) =
  let open Iter in
  let w1, w2 =
    let w1, w2 = Option.get_exn_or "UPDATE" watchers in
    if Literal.equal w1 l then (w1, w2) else (w2, w1)
  in
  let other_watcher_needs_updating =
    match Assignment.Map.find_opt w2 a with
    | Some ass
      when let l' = Assignment.literal ass in
           Literal.signum w2 <> Literal.signum l' ->
        true
    | _ -> false
  in
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
  let { index = index'; watcher1_change; watcher2_change; clause_falsified } =
    0 -- (size - 1)
    |> fold_while
         (fun {
                index = index';
                watcher1_change;
                watcher2_change;
                clause_falsified = falsified';
                _;
              } _ ->
           let index'' = (index' + 1) mod size in
           let l' = clause.(index') in
           match Assignment.Map.find_opt l' a with
           | Some ass
             when let l'' = Assignment.literal ass in
                  Literal.signum l' <> Literal.signum l'' ->
               ( {
                   index = index'';
                   watcher1_change;
                   watcher2_change;
                   clause_falsified = falsified';
                 },
                 `Continue )
           | _ -> (
               match watcher1_change with
               | None ->
                   ( {
                       index = index'';
                       watcher1_change =
                         (if Literal.equal w2 l' then None else Some l');
                       watcher2_change = None;
                       clause_falsified = false;
                     },
                     if other_watcher_needs_updating then `Continue else `Stop
                   )
               | Some l'' ->
                   ( {
                       index = index'';
                       watcher1_change;
                       watcher2_change =
                         (if Literal.equal l' l'' then None else Some l');
                       clause_falsified = false;
                     },
                     `Stop )))
         {
           index;
           watcher1_change = None;
           watcher2_change = None;
           clause_falsified = true;
         }
  in
  if clause_falsified then Falsified c
  else
    match (watcher1_change, watcher2_change) with
    | Some w1', Some w2' ->
        let c' = { c with watchers = Some (w1', w2'); index = index' } in
        WatcherChange (w1, w1', w2, w2', c')
    | Some w1', None when not other_watcher_needs_updating ->
        let c' = { c with watchers = Some (w1', w2); index = index' } in
        WatcherChange (w1, w1', w2, w2, c')
    | _ -> Unit c
(* TODO the problem here is that the clause might be satisfied and yet not be a unit clause, e.g.:

   lsat: [DEBUG] 1594 (watching 942 and -828 - index 0):
   mlsat: [DEBUG] -828(-828)
   mlsat: [DEBUG] 942(-942)
   mlsat: [DEBUG] Clause 1594 is ready for unit propagation
   mlsat: [ERROR] ASSERTION FAILED: Unit clauses are incorrect
*)
(* | Some _, None -> Unit c *)
(* | _ -> failwith "Impossible" *)

let watched_literals { watchers; _ } = watchers
