module M = struct
  type t = {
    id : int;
    clause : Clause.t;
    data : Literal.t Array.t;
    size : int;
    mutable index : int;
    mutable watched_literals : Literal.t * Literal.t;
  }

  let compare { id = id1; _ } { id = id2; _ } = compare id1 id2
end

type t = M.t
type watched_clause = t

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

  let remove l id m =
    let dummy =
      {
        id;
        clause = Clause.empty;
        data = Array.empty;
        size = 0;
        index = 0;
        watched_literals = (l, l);
      }
    in
    update m ~k:l ~f:(fun _ -> function
      | Some s -> Some (Set.remove dummy s) (* TODO *) | None -> None);
    m

  let show o =
    fold
      (fun l cs s ->
        Printf.sprintf "%s%s:%s\n" s (Literal.show l)
          (Set.fold (fun { id; _ } acc -> Printf.sprintf "%s%d " acc id) cs ""))
      o ""
end

type update_result =
  | WatchedLiteralChange of Map.t
  | Unit of (int * Clause.t)
  | Falsified of (int * Clause.t)
  | NoChange

let clause { id; clause; _ } = (id, clause)
let fold f x { data; _ } = Array.fold f x data

let of_clause a c id =
  let open Iter in
  let size = Clause.size c in

  let data = Clause.to_list c |> Array.of_list in
  Clause.to_iter c
  |> filter (fun l -> Tribool.is_nonfalse (Assignment.Map.value l a))
  |> take 2 |> to_list
  |> function
  | [ w1; w2 ] ->
      Some
        {
          id;
          clause = c;
          data;
          size;
          index = 2 mod size;
          watched_literals = (w1, w2);
        }
  | _ -> None

let update l a
    ({ id; clause; data; size; index; watched_literals = w1, w2; _ } as c)
    watchers =
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
          Falsified (id, clause)
        else Unit (id, clause)
    | Some (index', new_watched_literal) ->
        c.index <- index';
        c.watched_literals <- (other_watched_literal, new_watched_literal);
        let watchers' =
          Map.remove l id watchers |> Map.add new_watched_literal c
        in
        WatchedLiteralChange watchers'

let watched_literals { watched_literals; _ } = watched_literals
