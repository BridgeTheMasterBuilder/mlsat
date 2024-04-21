type trace = Addition of Clause.t | Deletion of Clause.t

type t = {
  (* TODO vector ? *)
  _debug_clauses : int list list;
  frequency : Frequency.Map.t;
  current_decision_level : int;
  assignments : Assignment.Map.t;
  (* TODO vector *)
  trail : (Assignment.t * t) list;
  (* TODO vector *)
  database : trace list;
  watchers : Clause.Watched.Map.t;
  unwatched : Clause.Set.t;
}

exception Conflict of Clause.t * t

module VariableWorkqueue : Workqueue.S with type elt = Variable.t =
  Workqueue.Make (Variable)

module UnitClauseWorkqueue : Workqueue.S with type elt = Literal.t * Clause.t =
Workqueue.Make (struct
  type t = Literal.t * Clause.t

  let compare (l1, _) (l2, _) = Literal.compare l1 l2
end)

let show
    ({
       _debug_clauses;
       frequency;
       current_decision_level;
       database;
       watchers;
       _;
     } as f) =
  Printf.sprintf
    "Clauses:\n\
     %s\n\
     Frequency:\n\
     %s\n\
     Decision level: %d\n\
     Assignments:\n\
     %s\n\
     Learned clauses:\n\
     %s\n\
     Watched literals:\n\
     %s\n"
    (if List.is_empty _debug_clauses then "()"
     else
       List.fold_left
         (fun acc c ->
           Printf.sprintf "%s( %s)\n" acc
             (List.fold_left (fun s l -> Printf.sprintf "%s%d " s l) "" c))
         "" _debug_clauses)
    (if Frequency.Map.is_empty frequency then "()"
     else Frequency.Map.show frequency)
    current_decision_level
    (List.fold_left
       (fun acc (ass, _) -> Printf.sprintf "%s%s" acc (Assignment.show ass))
       "" f.trail)
    (List.fold_left
       (fun acc c ->
         Printf.sprintf "%s( %s)\n" acc
           (match c with Addition c | Deletion c -> Clause.show c))
       "" database)
    (if Clause.Watched.Map.is_empty watchers then "()"
     else Clause.Watched.Map.show watchers)

let decision_level { current_decision_level = d; _ } = d

let rec make_assignment l ass
    ({ frequency; assignments = a; trail = t; _ } as f) d ucs =
  let update_watchers l ({ assignments = a; watchers; unwatched; _ } as f) ucs'
      =
    let update_watcher l c watchers' unwatched ucs'' =
      match Clause.Watched.update l a c watchers' with
      | WatchedLiteralChange watchers' ->
          ( watchers',
            (* TODO It might already not be watched, need a way to figure out when this is necessary, or maybe it doesn't matter? *)
            Clause.Set.remove (Clause.Watched.to_clause c) unwatched,
            ucs'' )
      | Unit (l, clause) ->
          (watchers', unwatched, UnitClauseWorkqueue.push (l, clause) ucs'')
      | Falsified clause -> raise_notrace (Conflict (clause, f))
      | NoChange -> (watchers', unwatched, ucs'')
    in
    match Clause.Watched.Map.find_opt l watchers with
    | Some cs ->
        let watchers', unwatched', ucs'' =
          Clause.Watched.Set.fold
            (fun c (watchers', unwatched', ucs'') ->
              update_watcher l c watchers' unwatched' ucs'')
            cs
            (watchers, unwatched, ucs')
        in
        let f' = { f with watchers = watchers'; unwatched = unwatched' } in
        (unit_propagate [@tailcall]) f' ucs''
    | None -> (unit_propagate [@tailcall]) f ucs'
  in
  let a' = Assignment.Map.add (Literal.var l) ass a in
  let t' = (ass, f) :: t in
  let frequency' =
    Frequency.Map.remove_literal l frequency
    |> Frequency.Map.remove_literal (Literal.neg l)
  in
  let f' =
    {
      f with
      assignments = a';
      trail = t';
      current_decision_level = d;
      frequency = frequency';
    }
  in
  update_watchers (Literal.neg l) f' ucs

and unit_propagate ({ current_decision_level = d; _ } as f) ucs =
  match UnitClauseWorkqueue.pop ucs with
  | None -> f
  | Some ((l, uc), ucs') ->
      let i =
        Assignment.Implication
          { literal = l; implicant = Clause.to_array uc; level = d }
      in
      make_assignment l i f d ucs'

let add_clause clause
    ({ frequency; assignments = a; watchers; unwatched; _ } as f) =
  let frequency' =
    let open Iter in
    Frequency.Map.incr_iter
      (Clause.to_iter clause
      |> filter (fun l -> not (Assignment.Map.mem (Literal.var l) a)))
      frequency
  in
  match Clause.Watched.watch_clause a clause watchers with
  | WatchedLiteralChange watchers' ->
      Ok
        {
          f with
          watchers = watchers';
          unwatched = Clause.Set.remove clause unwatched;
          frequency = frequency';
        }
  | Unit (l, clause) -> (
      let f' =
        {
          f with
          unwatched = Clause.Set.add clause unwatched;
          frequency = frequency';
        }
      in
      try Ok (unit_propagate f' (UnitClauseWorkqueue.singleton (l, clause)))
      with Conflict (c, f) -> Error (c, f))
  | Falsified clause -> Error (clause, f)
  | NoChange -> Ok { f with frequency = frequency' }

let analyze_conflict { current_decision_level = d; assignments = a; _ } clause =
  let open Iter in
  let rec aux q c =
    match VariableWorkqueue.pop_exn q with
    | l, q' -> (
        if VariableWorkqueue.is_empty q' then
          let ass = Assignment.Map.find l a in
          Literal.neg (Assignment.literal ass) :: c
        else
          match Assignment.(Map.find l a) with
          | Decision { literal = l'; _ } -> aux q' (Literal.neg l' :: c)
          | Implication { literal = l'; implicant = ls'; level = d'; _ } ->
              if d' < d then aux q' (Literal.neg l' :: c)
              else
                let q'' =
                  VariableWorkqueue.push_iter
                    (Array.to_iter ls' |> map Literal.var)
                    q'
                in
                aux q'' c)
  in
  let queue =
    let open Iter in
    Clause.to_iter clause
    |> map (fun l -> Assignment.Map.find (Literal.var l) a)
    |> sort ~cmp:Assignment.compare
    |> map (fun ass -> Literal.var (Assignment.literal ass))
    |> VariableWorkqueue.of_iter
  in
  aux queue [] |> Clause.of_list

let assignments { assignments = a; _ } = Assignment.Map.assignments a
let learned_clauses { database; _ } = database

(* let remove_clause clause ({ frequency; assignments = a; watchers; _ } as f) = *)
(*   let open Iter in *)
(*   let frequency' = *)
(*     Frequency.Map.decr_iter *)
(*       (Clause.to_iter clause *)
(*       |> filter (fun l -> not (Assignment.Map.mem (Literal.var l) a))) *)
(*       frequency *)
(*   in *)
(*   let watchers' = Clause.Watched.unwatch_clause clause watchers in *)
(*   { f with frequency = frequency'; watchers = watchers' } *)

let backtrack
    {
      assignments = a;
      trail = t;
      database = db;
      watchers;
      unwatched;
      frequency;
      _;
    } learned_clause =
  let open Result.Infix in
  let d' =
    let open Iter in
    Clause.to_iter learned_clause
    |> filter_map (fun l -> Assignment.Map.find_opt (Literal.var l) a)
    |> map Assignment.level
    |> sort ~cmp:(fun d1 d2 -> -compare d1 d2)
    |> drop 1 |> max |> Option.value ~default:0
  in
  let _, ({ frequency = frequency'; _ } as f) =
    if d' = 0 then List.last_opt t |> Option.get_exn_or "TRAIL"
    else List.find (fun (ass, _) -> Assignment.was_decided_on_level d' ass) t
  in
  let frequency'' = Frequency.Map.merge frequency frequency' in
  (* let frequency'' = frequency' in *)
  let f =
    {
      f with
      frequency = Frequency.Map.decay frequency'';
      watchers;
      database = Addition learned_clause :: db;
    }
  in
  let f' =
    Clause.Set.fold
      (fun clause f' -> add_clause clause f' |> Result.get_exn)
      unwatched f
  in
  let+ f'' = add_clause learned_clause f' in
  (f'', d')

let choose_literal { frequency; _ } = Frequency.Map.pop frequency
let is_empty { frequency; _ } = Frequency.Map.is_empty frequency

let of_list v _c list =
  let rec aux f = function
    | [] -> f
    | c :: cs ->
        (* TODO *)
        assert (List.to_iter c |> Iter.for_all (fun l -> l <> 0));
        let clause =
          let open Iter in
          List.to_iter c |> sort_uniq ~cmp:Int.compare
          |> map Literal.of_int_unchecked
          |> Clause.of_iter
        in
        if Clause.size clause = 0 then raise_notrace (Conflict (clause, f))
        else
          let f' =
            add_clause clause f
            |> Result.get_lazy (fun (c, f) -> raise_notrace (Conflict (c, f)))
          in
          aux f' cs
  in
  try
    Some
      (aux
         {
           _debug_clauses = list;
           frequency = Frequency.Map.empty ();
           current_decision_level = 0;
           assignments = Assignment.Map.empty ();
           trail = [];
           database = [];
           watchers = Clause.Watched.Map.make v;
           unwatched = Clause.Set.empty ();
         }
         list)
  with Conflict _ -> None

let restart ({ trail = t; watchers; database; unwatched; frequency; _ } as f) =
  if List.is_empty t then f
  else
    let ({ frequency = frequency'; _ } as f) =
      snd
        (List.last_opt t
        |> Option.get_exn_or
             "Internal solver error: Attempt to backtrack without previous \
              assignments.")
    in
    let frequency'' = Frequency.Map.merge frequency frequency' in
    (* let frequency'' = frequency' in *)
    let f = { f with watchers; database; frequency = frequency'' } in
    Clause.Set.fold
      (* TODO Is it safe to unwrap the result? *)
        (fun clause f' -> add_clause clause f' |> Result.get_exn)
      unwatched f

(* TODO do you need to track changes to frequency? *)
let eliminate_pure_literals ({ frequency; _ } as f) =
  let open Iter in
  Frequency.Map.to_iter frequency
  |> fold
       (fun f' (l, _) ->
         if Frequency.Map.mem (Literal.neg l) frequency then f'
         else
           let i =
             Assignment.Implication
               { literal = l; implicant = Array.empty; level = 0 }
           in
           make_assignment l i f' 0 (UnitClauseWorkqueue.empty ()))
       f

let preprocess f =
  let f = eliminate_pure_literals f in
  { f with trail = [] }

let make_decision ({ current_decision_level = d; _ } as f) =
  let l = choose_literal f in
  let dec = Assignment.Decision { literal = l; level = d + 1 } in
  try Ok (make_assignment l dec f (d + 1) (UnitClauseWorkqueue.empty ()))
  with Conflict (c, f) -> Error (c, f)

let check ({ database = db; trail; _ } as f) =
  Logs.debug (fun m -> m "%s" (show f));
  let db_equal =
    List.sort_uniq
      ~cmp:(fun c1 c2 ->
        let c1, c2 =
          match (c1, c2) with
          | (Addition c1 | Deletion c1), (Addition c2 | Deletion c2) -> (c1, c2)
        in
        Array.compare Literal.compare
          (Array.sorted Literal.compare (Clause.to_array c1))
          (Array.sorted Literal.compare (Clause.to_array c2)))
      db
    |> List.length = List.length db
  in
  if not db_equal then
    List.iter
      (fun (c1, c2) ->
        let c1, c2 =
          match (c1, c2) with
          | (Addition c1 | Deletion c1), (Addition c2 | Deletion c2) -> (c1, c2)
        in
        Logs.debug (fun m ->
            m "[%b ]%s = %s"
              (String.equal (Clause.show c1) (Clause.show c2))
              (Clause.show c1) (Clause.show c2)))
      (List.combine_shortest
         (List.sort_uniq
            ~cmp:(fun c1 c2 ->
              let c1, c2 =
                match (c1, c2) with
                | (Addition c1 | Deletion c1), (Addition c2 | Deletion c2) ->
                    (c1, c2)
              in
              Array.compare Literal.compare
                (Array.sorted Literal.compare (Clause.to_array c1))
                (Array.sorted Literal.compare (Clause.to_array c2)))
            db)
         (List.sort
            (fun c1 c2 ->
              let c1, c2 =
                match (c1, c2) with
                | (Addition c1 | Deletion c1), (Addition c2 | Deletion c2) ->
                    (c1, c2)
              in
              Array.compare Literal.compare
                (Array.sorted Literal.compare (Clause.to_array c1))
                (Array.sorted Literal.compare (Clause.to_array c2)))
            db));
  assert db_equal;
  let trail_equal =
    List.sort_uniq
      ~cmp:(fun (ass1, _) (ass2, _) ->
        Literal.compare (Assignment.literal ass1) (Assignment.literal ass2))
      trail
    |> List.length = List.length trail
  in
  if not trail_equal then
    List.iter
      (fun ((ass1, _), (ass2, _)) ->
        Logs.debug (fun m ->
            m "[%b] %s = %s"
              (String.equal (Assignment.show ass1) (Assignment.show ass2))
              (Assignment.show ass1) (Assignment.show ass2)))
      (List.combine_shortest
         (List.sort_uniq
            ~cmp:(fun (ass1, _) (ass2, _) -> Assignment.compare ass1 ass2)
            trail)
         (List.sort
            (fun (ass1, _) (ass2, _) -> Assignment.compare ass1 ass2)
            trail));
  assert trail_equal
