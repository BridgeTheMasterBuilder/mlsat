module M = struct
  type t = {
    id : int;
    iterator : Literal.t Iter.iter;
    clause : Clause.t;
    size : int;
    watchers : (Literal.t * Literal.t) option;
  }

  let compare { id = id1; _ } { id = id2; _ } = compare id1 id2
end

open M
module Set = Set.Make (M)

type t = M.t
type watched_clause = t

type update_result =
  | WatcherChange of (Literal.t * Literal.t * Literal.t * t)
  | Unit of t
  | Falsified of t
  | NoChange

module Map = struct
  module M = CCHashtbl.Make (Literal)
  include M

  type t = Set.t M.t
  type key = Literal.t

  let add l n m =
    update m ~k:l ~f:(fun _ -> function
      | Some s -> Some (Set.add n s) | None -> Some (Set.singleton n));
    m

  let empty = create 1024
  let find_opt l t = find_opt t l
  let is_empty m = length m = 0
  let mem l t = mem t l

  let remove l n m =
    update m ~k:l ~f:(fun _ -> function
      | Some s -> Some (Set.remove n s) | None -> failwith "REMOVE");
    m

  let show o =
    fold
      (fun l cs s ->
        Printf.sprintf "%s%s:%s\n" s (Literal.show l)
          (Set.fold (fun { id; _ } acc -> Printf.sprintf "%s%d " acc id) cs ""))
      o ""
end

let clause { id; clause; _ } = (id, clause)

let fold f x { iterator; size; _ } =
  let open Iter in
  take size iterator |> fold f x

let of_clause a c id =
  let open Iter in
  let clause_iter = Clause.to_iter c in
  let iterator = clause_iter |> cycle in
  let size = Clause.size c in
  let watchers =
    filter (fun l -> Tribool.is_nonfalse (Assignment.Map.value l a)) clause_iter
    |> take 2 |> to_list
    |> function
    | [ w1; w2 ] -> Some (w1, w2)
    | _ -> None
  in
  { id; iterator; clause = c; size; watchers }

let update l a ({ iterator; size; watchers; _ } as c) =
  let open CCEither in
  let open Iter in
  let w1, w2 = Option.get_exn_or "UPDATE" watchers in
  let other_watcher, other_watcher_literal =
    if Literal.equal l w1 then (Right w2, w2) else (Left w1, w1)
  in
  let other_watcher_truth_value =
    Assignment.Map.value other_watcher_literal a
  in
  if Tribool.is_true other_watcher_truth_value then NoChange
  else
    let result =
      take size iterator
      |> find_map (fun l' ->
             if
               Tribool.is_false (Assignment.Map.value l' a)
               || Literal.equal l' other_watcher_literal
             then None
             else Some l')
    in
    match result with
    | None ->
        if Tribool.is_false other_watcher_truth_value then Falsified c
        else Unit c
    | Some new_watcher ->
        let c' =
          {
            c with
            watchers =
              (match other_watcher with
              | Left w -> Some (w, new_watcher)
              | Right w -> Some (new_watcher, w));
          }
        in
        WatcherChange (l, new_watcher, other_watcher_literal, c')

let watched_literals { watchers; _ } = watchers
