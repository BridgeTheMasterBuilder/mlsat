type t = {
  _debug_clauses : int list list;
  frequency : Frequency.Map.t;
  current_decision_level : int;
  assignments : Assignment.Map.t;
  trail : (Assignment.t * t) list;
  database : Database.t;
  watchers : Watched.Literal.Map.t;
  unwatched : Clause.Set.t;
}

exception Conflict of Clause.t * t

module VariableWorkqueue : Workqueue.S with type elt = Variable.t =
  Workqueue.Make (Variable)

module UnitClauseWorkqueue : Workqueue.S with type elt = Literal.t * Clause.t =
Workqueue.Make (struct
  type t = Literal.t * Clause.t

  let equal (l1, _) (l2, _) = Literal.equal l1 l2
  let hash (l, _) = Literal.hash l
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
    (Database.fold
       (fun acc c -> Printf.sprintf "%s( %s)\n" acc (Clause.show c))
       "" database)
    (if Watched.Literal.Map.is_empty watchers then "()"
     else Watched.Literal.Map.show watchers)

let decision_level { current_decision_level = d; _ } = d

let rec make_assignment l ass ({ assignments = a; trail = t; _ } as f) d ucs =
  let update_watchers l ({ assignments = a; watchers; unwatched; _ } as f) ucs'
      =
    let update_watcher l c watchers' unwatched ucs'' =
      match Watched.Clause.update l a c watchers' with
      | WatchedLiteralChange watchers' ->
          ( watchers',
            (* TODO It might already not be watched, need a way to figure out when this is necessary, or maybe it doesn't matter? *)
            Clause.Set.remove (Watched.Clause.to_clause c) unwatched,
            ucs'' )
      | Unit (l, clause) ->
          (watchers', unwatched, UnitClauseWorkqueue.push (l, clause) ucs'')
      | Falsified clause -> raise_notrace (Conflict (clause, f))
      | NoChange -> (watchers', unwatched, ucs'')
    in
    match Watched.Literal.Map.find_opt l watchers with
    | Some cs ->
        let watchers', unwatched', ucs'' =
          Watched.Clause.Set.fold
            (fun (watchers', unwatched', ucs'') c ->
              update_watcher l c watchers' unwatched' ucs'')
            (watchers, unwatched, ucs')
            cs
        in
        let f' = { f with watchers = watchers'; unwatched = unwatched' } in
        (unit_propagate [@tailcall]) f' ucs''
    | None -> (unit_propagate [@tailcall]) f ucs'
  in
  let a' = Assignment.Map.add (Literal.var l) ass a in
  let t' = (ass, f) :: t in
  let f' =
    { f with assignments = a'; trail = t'; current_decision_level = d }
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
  match Watched.Clause.watch a clause watchers with
  | WatchedLiteralChange watchers' ->
      {
        f with
        watchers = watchers';
        unwatched = Clause.Set.remove clause unwatched;
        frequency = frequency';
      }
  | Unit (l, clause) ->
      let f' =
        {
          f with
          unwatched = Clause.Set.add clause unwatched;
          frequency = frequency';
        }
      in
      unit_propagate f' (UnitClauseWorkqueue.singleton (l, clause))
  | Falsified clause -> raise_notrace (Conflict (clause, f))
  | NoChange -> { f with frequency = frequency' }

let analyze_conflict
    ({ current_decision_level = d; assignments = a; database = db; _ } as f)
    clause =
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
  let id, db' = Database.new_id db in
  let learned_clause = aux queue [] |> Clause.of_list id in
  (learned_clause, { f with database = db' })

let assignments { assignments = a; _ } = Assignment.Map.assignments a
let trace { database; _ } = Database.get_trace database

let forget_clauses ({ database = db; _ } as f) =
  let db' = Database.delete_half db in
  let f' = f in
  { f' with database = db' }

let _remove_clause clause ({ frequency; assignments = a; watchers; _ } as f) =
  let open Iter in
  let frequency' =
    Frequency.Map.decr_iter
      (Clause.to_iter clause
      |> filter (fun l -> not (Assignment.Map.mem (Literal.var l) a)))
      frequency
  in
  let watchers' = Watched.Clause.unwatch clause watchers in
  { f with frequency = frequency'; watchers = watchers' }

let backtrack
    { assignments = a; trail = t; database = db; watchers; unwatched; _ }
    learned_clause =
  let d' =
    let open Iter in
    Clause.to_iter learned_clause
    |> filter_map (fun l -> Assignment.Map.find_opt (Literal.var l) a)
    |> map Assignment.level
    |> sort ~cmp:(fun d1 d2 -> -compare d1 d2)
    |> drop 1 |> max |> Option.value ~default:0
  in
  let _, ({ frequency; _ } as f) =
    if d' = 0 then List.last_opt t |> Option.get_exn_or "TRAIL"
    else List.find (fun (ass, _) -> Assignment.was_decided_on_level d' ass) t
  in
  let f =
    {
      f with
      frequency = Frequency.Map.decay frequency;
      watchers;
      database = Database.add_clause learned_clause db;
    }
  in
  let f' =
    Clause.Set.fold (fun f' clause -> add_clause clause f') f unwatched
  in
  try
    let f'' = add_clause learned_clause f' in
    Ok (f'', d')
  with Conflict (c, f) -> Error (c, f)

let choose_literal { frequency; _ } = Frequency.Map.min_exn frequency

let is_empty ({ frequency; assignments = a; _ } as f) =
  let frequency' = Frequency.Map.flush_assigned a frequency in
  let f' = { f with frequency = frequency' } in
  let open Either in
  if Frequency.Map.is_empty frequency' then Right f' else Left f'

let of_list v c list =
  let rec aux n f = function
    | [] -> f
    | c :: cs ->
        (* TODO *)
        assert (List.to_iter c |> Iter.for_all (fun l -> l <> 0));
        let clause =
          let open Iter in
          List.to_iter c |> sort_uniq ~cmp:Int.compare
          |> map Literal.of_int_unchecked
          |> Clause.of_iter n
        in
        if Clause.size clause = 0 then raise_notrace (Conflict (clause, f))
        else
          let f' = add_clause clause f in
          aux (n + 1) f' cs
  in
  try
    Some
      (aux 0
         {
           _debug_clauses = list;
           frequency = Frequency.Map.empty ();
           current_decision_level = 0;
           assignments = Assignment.Map.empty ();
           trail = [];
           database = Database.create c;
           watchers = Watched.Literal.Map.make v;
           unwatched = Clause.Set.empty ();
         }
         list)
  with Conflict _ -> None

let restart ({ trail = t; watchers; database; unwatched; _ } as f) =
  if List.is_empty t then f
  else
    let ({ frequency; _ } as f) =
      snd
        (List.last_opt t
        |> Option.get_exn_or
             "Internal solver error: Attempt to backtrack without previous \
              assignments.")
    in
    let f = { f with watchers; database; frequency } in
    Clause.Set.fold (fun f' clause -> add_clause clause f') f unwatched

let eliminate_pure_literals ({ frequency; _ } as f) =
  let open Iter in
  Frequency.Map.to_iter frequency
  |> fold
       (fun ({ frequency = frequency'; _ } as f') (l, _) ->
         if Frequency.Map.mem (Literal.neg l) frequency' then f'
         else
           let i =
             Assignment.Implication
               { literal = l; implicant = Array.empty; level = 0 }
           in
           let frequency'' = Frequency.Map.remove_literal l frequency in
           let f' = { f' with frequency = frequency'' } in
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
  Database.check db;
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
