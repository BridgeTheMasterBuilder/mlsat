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
  let watchers = if size >= 2 then Some (clause.(0), clause.(1)) else None in
  { id; clause; size; index = 2 mod size; watchers }

type update_result =
  | WatcherChange of (Literal.t * Literal.t * Literal.t * t)
  | Unit of t
  | Falsified of t

(* TODO the problem here is that we may need to update the other watcher too, I'm struggling
        to find a clean design for that *)
let update l a ({ clause; size; index; watchers; _ } as c) =
  let update_watchers w w' =
    let w1, w2 = Option.get_exn_or "UPDATE_WATCHERS" watchers in
    if Literal.equal w' w1 || Literal.equal w' w2 then None
      (* else Some (if Literal.equal w w1 then (w', w2) else (w1, w')) *)
    else Some (w, w', if Literal.equal w w1 then w2 else w1)
  in
  let open Iter in
  let w1, w2 = Option.get_exn_or "UPDATE" watchers in
  Logs.debug (fun m ->
      m "%d (watching %s and %s - index %d): " c.id (Literal.show w1)
        (Literal.show w2) index);
  let index', watcher_change, falsified =
    Array.iter
      (fun l ->
        Logs.debug (fun m ->
            m "%s(%s) " (Literal.show l)
              (Assignment.Map.find_opt l a
              |> Option.map_or ~default:"_" (fun ass ->
                     Literal.show (Assignment.literal ass)))))
      c.clause;
    0 -- (size - 1)
    |> fold_while
         (fun (index', _, falsified') _ ->
           let index'' = (index' + 1) mod size in
           let l' = clause.(index') in
           match Assignment.Map.find_opt l' a with
           (* | Some ass -> *)
           (*     let l'' = Assignment.literal ass in *)
           (*     let falso = Literal.signum l' <> Literal.signum l'' in *)
           (*     if falso then ((index'', None, falsified' && falso), `Continue) *)
           (*     else *)
           (*       let watcher_change' = update_watchers l l' in *)
           (*       ((index'', watcher_change', false), `Stop) *)
           | Some ass
             when let l'' = Assignment.literal ass in
                  Literal.signum l' <> Literal.signum l'' ->
               ((index'', None, falsified'), `Continue)
           | _ -> (
               (* TODO *)
               match update_watchers l l' with
               | None -> ((index'', None, false), `Continue)
               | watcher_change' ->
                   (* |> Option.map (fun watchers' -> (w1, w1', w2, watchers')) *)
                   ((index'', watcher_change', false), `Stop)))
         (index, None, true)
  in
  if falsified then Falsified c
  else
    (* TODO *)
    match watcher_change with
    | Some (w1, w1', w2) ->
        let c' = { c with watchers = Some (w1', w2); index = index' } in
        (* assert ( *)
        (*   Assignment.Map.find_opt w1' a *)
        (*   |> Option.map_or ~default:true (fun ass -> *)
        (*          let l' = Assignment.literal ass in *)
        (*          Printf.printf "%s = %s\n" (Literal.show w1') (Literal.show l'); *)
        (*          Literal.signum w1' = Literal.signum l') *)
        (*   && Assignment.Map.find_opt w2 a *)
        (*      |> Option.map_or ~default:true (fun ass -> *)
        (*             let l' = Assignment.literal ass in *)
        (*             Printf.printf "%s = %s\n" (Literal.show w2) *)
        (*               (Literal.show l'); *)
        (*             Literal.signum w2 = Literal.signum l')); *)
        (* let _w1', _w2 = Option.get_exn_or "WATCHERS" c'.watchers in *)
        (* Printf.printf "%d (watching %s and %s - index %d)\n\n" c'.id *)
        (*   (Literal.show _w1') (Literal.show _w2) index'; *)
        WatcherChange (w1, w1', w2, c')
    | None -> Unit c
(* TODO False positive

   Making assignment       6 because of clause ( 6 25 47 ) - implied at level 11

   52 (watching -36 and -6 - index 2): -36(-36) -6(6) 45(45)
   Clause 52 is ready for unit propagation
   6 (watching -32 and -6 - index 2): -32(-32) -6(6) 15(_)
   6 (watching 15 and -32 - index 0)
*)

let watched_literals { watchers; _ } = watchers
