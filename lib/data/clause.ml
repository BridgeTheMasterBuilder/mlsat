include CCArray

type t = Literal.t array
type clause = t

(* TODO lbd a c *)
let of_array = Fun.id
let size = length
let show c = fold (fun s l -> Printf.sprintf "%s%s " s (Literal.show l)) "" c
let to_array = Fun.id

module Watched = struct
  module Clause = struct
    type t = {
      id : int;
      clause : clause;
      size : int;
      mutable index : int;
      mutable watched_literals : Literal.t * Literal.t;
    }

    let compare { id = id1; _ } { id = id2; _ } = Int.compare id1 id2
  end

  type t = Clause.t
  type watched_clause = t

  open Clause
  module Set = Set.Make (Clause)

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

    let remove l c m =
      update m ~k:l ~f:(fun _ -> function
        | Some s -> Some (Set.remove c s) (* TODO *) | None -> None);
      m

    let show o =
      fold
        (fun l cs s ->
          Printf.sprintf "%s%s:%s\n" s (Literal.show l)
            (Set.fold
               (fun { id; _ } acc -> Printf.sprintf "%s%d " acc id)
               cs ""))
        o ""
  end

  type update_result =
    | WatchedLiteralChange of (watched_clause * Map.t)
    | Unit of (Literal.t * clause)
    | Falsified of clause
    | NoChange

  let fold f x { clause; _ } = clause |> Array.fold f x

  let watch_clause a c id watchers =
    let size = size c in
    to_iter c
    |> Iter.filter (fun l -> Tribool.is_nonfalse (Assignment.Map.value l a))
    |> Iter.take 2 |> Iter.to_list
    |> function
    | [ w1; w2 ] ->
        let watched_clause =
          {
            id;
            clause = c;
            size;
            index = 2 mod size;
            watched_literals = (w1, w2);
          }
        in
        let watchers' =
          Map.add w1 watched_clause watchers |> Map.add w2 watched_clause
        in
        WatchedLiteralChange (watched_clause, watchers')
    | [ w ] ->
        if Tribool.is_unknown (Assignment.Map.value w a) then Unit (w, c)
        else NoChange
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
          WatchedLiteralChange (c, watchers')
end

module Map = struct
  open Batteries

  type t = Watched.t BatDynArray.t
  type key = int

  let add c m =
    BatDynArray.add m c;
    m

  let find = Fun.flip BatDynArray.unsafe_get
  let is_empty = BatDynArray.empty
  let make = BatDynArray.make

  let remove n m =
    BatDynArray.delete m n;
    m

  let show =
    let open Watched.Clause in
    BatDynArray.fold_lefti
      (fun s c { clause; _ } -> Printf.sprintf "%s%d:%s\n" s c (show clause))
      ""

  let size = BatDynArray.length

  let to_iter m =
    BatDynArray.to_list m |> Iter.of_list |> Iter.mapi (fun i x -> (i, x))
end
