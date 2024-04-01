module M = struct
  type t = {
    id : int;
    data : Literal.t array;
    clause : Clause.t;
    size : int;
    mutable index : int;
    mutable watchers : Literal.t * Literal.t;
  }

  let compare { id = id1; _ } { id = id2; _ } = compare id1 id2
end

type t = M.t
type watched_clause = t

type update_result =
  | WatcherChange of (Literal.t * Literal.t * t)
  | Unit
  | Falsified
  | NoChange

open M
module Set = Set.Make (M)

module Map = struct
  module M = CCHashtbl.Make (Literal)
  include M

  type t = Set.t M.t
  type key = Literal.t

  let add l n m =
    update m ~k:l ~f:(fun _ -> function
      | Some s -> Some (Set.add n s) | None -> Some (Set.singleton n));
    m

  let make = create
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
let fold f x { data; _ } = Array.fold f x data

let of_clause a c id =
  let open Iter in
  let clause_iter = Clause.to_iter c in
  let data = clause_iter |> to_array in
  let size = Array.length data in
  filter (fun l -> Tribool.is_nonfalse (Assignment.Map.value l a)) clause_iter
  |> take 2 |> to_list
  |> function
  | [ w1; w2 ] ->
      Some
        { id; data; clause = c; size; index = 2 mod size; watchers = (w1, w2) }
  | _ -> None

let update l a ({ data; size; index; watchers = w1, w2; _ } as c) =
  let open Iter in
  let other_watcher = if Literal.equal l w1 then w2 else w1 in
  let other_watcher_truth_value = Assignment.Map.value other_watcher a in
  if Tribool.is_true other_watcher_truth_value then NoChange
  else
    let result =
      0 -- (size - 1)
      |> find_map (fun i ->
             let index' = (index + i) mod size in
             let l' = Array.unsafe_get data index' in
             if
               Tribool.is_false (Assignment.Map.value l' a)
               || Literal.equal l' other_watcher
             then None
             else Some (index', l'))
    in
    match result with
    | None ->
        if Tribool.is_false other_watcher_truth_value then Falsified else Unit
    | Some (index', new_watcher) ->
        c.index <- index';
        c.watchers <- (other_watcher, new_watcher);
        (* let c' = *)
        (*   { *)
        (*     c with *)
        (*     index = index'; *)
        (*     watchers = *)
        (*       (match other_watcher with *)
        (*       | Left w -> *)
        (*           c.watchers <- Some (w, new_watcher); *)
        (*           Some (w, new_watcher) *)
        (*       | Right w -> *)
        (*           c.watchers <- Some (w, new_watcher); *)
        (*           Some (new_watcher, w)); *)
        (*   } *)
        (* in *)
        WatcherChange (l, new_watcher, c)

let watched_literals { watchers; _ } = watchers
