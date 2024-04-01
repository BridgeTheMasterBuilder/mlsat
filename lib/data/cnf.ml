type formula = {
  clauses : Clause.Map.t;
  frequency : Frequency.Map.t;
  unit_clauses : UnitClauseQueue.t;
  current_decision_level : int;
  assignments : Assignment.Map.t;
  trail : (Assignment.t * formula) list;
  (* TODO rename to trace and include clause deletion too *)
  database : Clause.t list;
  watchers : WatchedClause.Map.t;
}

exception Conflict of Clause.t * formula

module VariableWorkqueue : Workqueue.S with type elt = Variable.t =
  Workqueue.Make (Variable)

let show
    ({
       clauses;
       frequency;
       current_decision_level;
       database;
       watchers;
       unit_clauses;
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
     %s\n\
     Unit clauses:\n\
     %s\n"
    (if Clause.Map.is_empty clauses then "()" else Clause.Map.show clauses)
    (if Frequency.Map.is_empty frequency then "()"
     else Frequency.Map.show frequency)
    current_decision_level
    (List.fold_left
       (fun acc (ass, _) -> Printf.sprintf "%s%s" acc (Assignment.show ass))
       "" f.trail)
    (List.fold_left
       (fun acc c -> Printf.sprintf "%s( %s)\n" acc (Clause.show c))
       "" database)
    (if WatchedClause.Map.is_empty watchers then "()"
     else WatchedClause.Map.show watchers)
    (UnitClauseQueue.fold
       (fun acc (_, c) -> Printf.sprintf "%s( %s)\n" acc (Clause.show c))
       "" unit_clauses)

let add_clause clause
    ({ clauses; frequency; unit_clauses = uc; assignments = a; watchers; _ } as
    f) =
  let n = Clause.Map.size clauses in
  let clauses' = Clause.Map.add clause clauses in
  let frequency' =
    let open Iter in
    Frequency.Map.incr_iter
      (Clause.to_iter clause
      |> filter (fun l -> not (Assignment.Map.mem (Literal.var l) a)))
      frequency
  in
  let watchers', uc' =
    match WatchedClause.of_clause a clause n with
    | Some watched_clause ->
        let w1, w2 = WatchedClause.watched_literals watched_clause in
        ( WatchedClause.Map.add w1 watched_clause watchers
          |> WatchedClause.Map.add w2 watched_clause,
          uc )
    | None -> (watchers, UnitClauseQueue.snoc uc (n, clause))
  in
  {
    f with
    clauses = clauses';
    frequency = frequency';
    unit_clauses = uc';
    watchers = watchers';
  }

let analyze_conflict { current_decision_level = d; assignments = a; _ } clause =
  let open Iter in
  let ls = Clause.to_list clause in
  let rec aux q c =
    match VariableWorkqueue.pop q with
    | None -> c
    | Some (l, q') -> (
        if VariableWorkqueue.is_empty q' then
          let ass = Assignment.Map.find l a in
          Clause.add (Literal.neg (Assignment.literal ass)) c
        else
          match Assignment.(Map.find_opt l a) with
          | Some (Decision { literal = l'; _ }) ->
              aux q' (Clause.add (Literal.neg l') c)
          | Some (Implication { literal = l'; implicant = ls'; level = d'; _ })
            ->
              if d' < d then aux q' (Clause.add (Literal.neg l') c)
              else
                let q'' =
                  VariableWorkqueue.push_iter
                    (Clause.to_iter ls' |> map Literal.var)
                    q'
                in
                aux q'' c
          | None -> aux q' c)
  in
  aux
    (VariableWorkqueue.of_iter (List.to_iter ls |> map Literal.var))
    Clause.empty

let assignments { assignments = a; _ } = Assignment.Map.assignments a
let learned_clauses { database; _ } = database

let backtrack
    {
      clauses;
      assignments = a;
      trail = t;
      database = db;
      watchers;
      unit_clauses = uc;
      _;
    } learned_clause =
  let d' =
    let open Iter in
    Clause.to_iter learned_clause
    |> filter_map (fun l -> Assignment.Map.find_opt (Literal.var l) a)
    |> map Assignment.level
    |> sort ~cmp:(fun d1 d2 -> -compare d1 d2)
    |> drop 1 |> max |> Option.value ~default:0
  in
  let _, f' =
    if d' = 0 then List.last_opt t |> Option.get_exn_or "TRAIL"
    else List.find (fun (ass, _) -> Assignment.was_decided_on_level ass d') t
  in
  let f' =
    {
      f' with
      clauses;
      frequency = Frequency.Map.decay f'.frequency;
      watchers;
      database = learned_clause :: db;
      unit_clauses = UnitClauseQueue.clear uc;
    }
  in
  let f'' = add_clause learned_clause f' in
  (f'', d')

let choose_literal { frequency; _ } = Frequency.Map.pop frequency
let is_empty { frequency; _ } = Frequency.Map.is_empty frequency

let of_list v c =
  let rec aux f = function
    | [] -> f
    | c :: cs ->
        let clause = Clause.of_int_list c in
        let f' = add_clause clause f in
        aux f' cs
  in
  aux
    {
      clauses = Clause.Map.make c;
      frequency = Frequency.Map.empty;
      current_decision_level = 0;
      assignments = Assignment.Map.empty;
      trail = [];
      database = [];
      unit_clauses = UnitClauseQueue.empty;
      watchers = WatchedClause.Map.make v;
    }

let remove_clause (n, clause)
    ({ clauses; frequency; assignments = a; watchers; _ } as f) =
  let clauses' = Clause.Map.remove n clauses in
  (* TODO *)
  let frequency' =
    let open Iter in
    Frequency.Map.decr_iter
      (Clause.to_iter clause
      |> filter (fun l -> not (Assignment.Map.mem (Literal.var l) a)))
      frequency
  in
  let watchers' =
    watchers
    (* match WatchedClause.watched_literals watched_clause with *)
    (* | Some (l1, l2) -> *)
    (*     WatchedClause.Map.add l1 watched_clause watchers *)
    (*     |> WatchedClause.Map.add l2 watched_clause *)
    (* | None -> watchers *)
  in
  { f with clauses = clauses'; frequency = frequency'; watchers = watchers' }

let restart
    ({ clauses; trail = t; watchers; database = db; unit_clauses = uc; _ } as f)
    =
  if List.is_empty t then f
  else
    let f' =
      snd
        (List.last_opt t
        |> Option.get_exn_or
             "Attempt to backtrack without previous assignments.")
    in
    {
      f' with
      clauses;
      watchers;
      database = db;
      unit_clauses = UnitClauseQueue.clear uc;
    }

let update_watchers l ({ assignments = a; watchers; _ } as f) =
  let update_watcher l c ({ unit_clauses = uc'; watchers = watchers'; _ } as f')
      =
    let n, ls = WatchedClause.clause c in
    match WatchedClause.update l a c with
    | WatcherChange (w, w', c') ->
        let watchers' =
          WatchedClause.Map.remove w c watchers' |> WatchedClause.Map.add w' c'
        in
        { f' with watchers = watchers' }
    | Unit -> { f' with unit_clauses = UnitClauseQueue.snoc uc' (n, ls) }
    | Falsified -> raise_notrace (Conflict (ls, f))
    | NoChange -> f'
  in
  match WatchedClause.Map.find_opt l watchers with
  | Some cs -> WatchedClause.Set.fold (update_watcher l) cs f
  | None -> f

let make_assignment l ass ({ frequency; assignments = a; trail = t; _ } as f) =
  let a' = Assignment.Map.add (Literal.var l) ass a in
  let t' = (ass, f) :: t in
  let f = { f with assignments = a'; trail = t' } in
  try
    let f' = update_watchers (Literal.neg l) f in
    let frequency' =
      Frequency.Map.remove_literal l frequency
      |> Frequency.Map.remove_literal (Literal.neg l)
    in
    let f' = { f' with frequency = frequency' } in
    Ok f'
  with Conflict (c, f) -> Error (c, f)

let eliminate_pure_literals ({ frequency; _ } as f) =
  let f' =
    let open Iter in
    Frequency.Map.to_iter frequency
    |> fold
         (fun ({ frequency; _ } as f') (l, _) ->
           if Frequency.Map.mem (Literal.neg l) frequency then f'
           else
             let i =
               Assignment.Implication
                 { literal = l; implicant = Clause.empty; level = 0 }
             in
             make_assignment l i f' |> Result.get_exn)
         f
  in
  { f' with trail = [] }

let preprocess = eliminate_pure_literals

let rec unit_propagate
    ({ unit_clauses = ucs; assignments = a; current_decision_level = d; _ } as
    f) =
  match UnitClauseQueue.take_front ucs with
  | Some ((_, uc), ucs') -> (
      let f' = { f with unit_clauses = ucs' } in
      match
        Clause.to_iter uc
        |> Iter.find_pred (fun l -> not (Assignment.Map.mem (Literal.var l) a))
      with
      | Some l ->
          let i =
            Assignment.Implication { literal = l; implicant = uc; level = d }
          in
          make_assignment l i f' |> Result.flat_map unit_propagate
      | None -> unit_propagate f')
  | None -> Ok f

let make_decision ({ current_decision_level = d; _ } as f) =
  let l = choose_literal f in
  let dec = Assignment.Decision { literal = l; level = d + 1 } in
  make_assignment l dec f
  |> Result.map (fun f' -> { f' with current_decision_level = d + 1 })
