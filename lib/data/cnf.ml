type formula = {
  clauses : Clause.Map.t;
  frequency : Frequency.Map.t;
  unit_clauses : UnitClauseQueue.t;
  current_decision_level : int;
  assignments : Assignment.Map.t;
  trail : (Assignment.t * formula) list;
  (* TODO rename to trace and include clause deletion too *)
  database : (int * Clause.t) list;
  watchers : Clause.Watched.Map.t;
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
       (fun acc (_, c) -> Printf.sprintf "%s( %s)\n" acc (Clause.show c))
       "" database)
    (if Clause.Watched.Map.is_empty watchers then "()"
     else Clause.Watched.Map.show watchers)
    (UnitClauseQueue.fold
       (fun acc (_, c) -> Printf.sprintf "%s( %s)\n" acc (Clause.show c))
       "" unit_clauses)

let rec make_assignment l ass
    ({ frequency; assignments = a; trail = t; _ } as f) =
  let update_watchers l ({ assignments = a; watchers; _ } as f) =
    (* TODO inline? *)
    let update_watcher l c ({ watchers = watchers'; _ } as f') =
      match Clause.Watched.update l a c watchers' with
      | WatchedLiteralChange (_, watchers') ->
          Ok { f' with watchers = watchers' }
      | Unit (l, uc) -> unit_propagate (l, uc) f'
      | Falsified ls -> Error (ls, f)
      | NoChange -> Ok f'
    in
    try
      Ok
        (match Clause.Watched.Map.find_opt l watchers with
        | Some cs ->
            Clause.Watched.Set.fold
              (fun c f' ->
                match update_watcher l c f' with
                | Ok f' -> f'
                | Error (c, f) -> raise_notrace (Conflict (c, f)))
              cs f
        | None -> f)
    with Conflict (c, f) -> Error (c, f)
  in
  let a' = Assignment.Map.add (Literal.var l) ass a in
  let t' = (ass, f) :: t in
  let f = { f with assignments = a'; trail = t' } in
  update_watchers (Literal.neg l) f
  |> Result.map (fun f' ->
         let frequency' =
           Frequency.Map.remove_literal l frequency
           |> Frequency.Map.remove_literal (Literal.neg l)
         in
         { f' with frequency = frequency' })

and unit_propagate (l, uc) ({ current_decision_level = d; _ } as f) =
  let f' = f in
  let i =
    Assignment.Implication
      { literal = l; implicant = Clause.to_array uc; level = d }
  in
  make_assignment l i f'

let add_clause (n, clause)
    ({ clauses; frequency; assignments = a; watchers; _ } as f) =
  let frequency' =
    let open Iter in
    Frequency.Map.incr_iter
      (Clause.to_iter clause
      |> filter (fun l -> not (Assignment.Map.mem (Literal.var l) a)))
      frequency
  in
  let watchers', f', clauses' =
    match Clause.Watched.watch_clause a clause n watchers with
    | WatchedLiteralChange (watched_clause, watchers') ->
        let clauses' = Clause.Map.add watched_clause clauses in
        (watchers', Ok f, clauses')
    | Unit (l, clause) -> (watchers, unit_propagate (l, clause) f, clauses)
    | Falsified clause -> (watchers, Error (clause, f), clauses)
    | NoChange -> (watchers, Ok f, clauses)
  in
  Result.map
    (fun f' ->
      {
        f' with
        clauses = clauses';
        frequency = frequency';
        watchers = watchers';
      })
    f'

let analyze_conflict { current_decision_level = d; assignments = a; _ } clause =
  let open Iter in
  let rec aux q c =
    match VariableWorkqueue.pop q with
    | None -> c
    | Some (l, q') -> (
        if VariableWorkqueue.is_empty q' then
          let ass = Assignment.Map.find l a in
          Literal.neg (Assignment.literal ass) :: c
        else
          match Assignment.(Map.find_opt l a) with
          | Some (Decision { literal = l'; _ }) -> aux q' (Literal.neg l' :: c)
          | Some (Implication { literal = l'; implicant = ls'; level = d'; _ })
            ->
              if d' < d then aux q' (Literal.neg l' :: c)
              else
                let q'' =
                  VariableWorkqueue.push_iter
                    (Array.to_iter ls' |> map Literal.var)
                    q'
                in
                aux q'' c
          | None -> aux q' c)
  in
  aux (VariableWorkqueue.of_iter (Clause.to_iter clause |> map Literal.var)) []
  |> Clause.of_list

let assignments { assignments = a; _ } = Assignment.Map.assignments a
let learned_clauses { database; _ } = List.map snd database

let remove_clause (n, clause)
    ({ clauses; frequency; assignments = a; watchers; _ } as f) =
  let open Iter in
  let clauses' = Clause.Map.remove n clauses in
  let frequency' =
    Frequency.Map.decr_iter
      (Clause.to_iter clause
      |> filter (fun l -> not (Assignment.Map.mem (Literal.var l) a)))
      frequency
  in
  let watchers' = Clause.Watched.unwatch_clause clause watchers in
  { f with clauses = clauses'; frequency = frequency'; watchers = watchers' }

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
  let n = Clause.Map.size clauses in
  let f' =
    {
      f' with
      clauses;
      frequency = Frequency.Map.decay f'.frequency;
      watchers;
      database = (n, learned_clause) :: db;
      unit_clauses = UnitClauseQueue.clear uc;
    }
  in

  (* let f' = add_clause (n, learned_clause) f' in *)
  (* let f' = remove_clause (n, learned_clause) f' in *)
  (* let f'' = add_clause (n, learned_clause) f' in *)
  add_clause (n, learned_clause) f' |> Result.map (fun f'' -> (f'', d'))

let choose_literal { frequency; _ } = Frequency.Map.pop frequency
let is_empty { frequency; _ } = Frequency.Map.is_empty frequency

let of_list v c list =
  let rec aux ({ clauses; _ } as f) = function
    | [] -> f
    | c :: cs ->
        assert (List.to_iter c |> Iter.for_all (fun l -> l <> 0));
        let clause = Clause.of_list (Literal.List.of_int_list c) in
        if Clause.size clause = 0 then raise_notrace (Conflict (clause, f))
        else
          let n = Clause.Map.size clauses in
          let f' =
            add_clause (n, clause) f
            |> Result.get_lazy (fun (c, f) -> raise_notrace (Conflict (c, f)))
          in
          aux f' cs
  in

  try
    Some
      (aux
         {
           (* TODO tweak *)
           clauses = Clause.Map.make (c * 2);
           frequency = Frequency.Map.empty;
           current_decision_level = 0;
           assignments = Assignment.Map.empty;
           trail = [];
           database = [];
           unit_clauses = UnitClauseQueue.empty;
           watchers = Clause.Watched.Map.make v;
         }
         list)
  with Conflict _ -> None

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
                 { literal = l; implicant = Array.empty; level = 0 }
             in
             make_assignment l i f' |> Result.get_exn)
         f
  in
  { f' with trail = [] }

let preprocess = eliminate_pure_literals

(* TODO Fix inconsistent unit propagations, e.g.

   mlsat: [DEBUG] Clauses:
   ()
   Frequency:
   -1:1.000000
   1:1.000000

   Decision level: 0
   Assignments:

   Learned clauses:

   Watched literals:
   ()
   Unit clauses:
   ( 1 )
   ( -1 )


   s SATISFIABLE
   v -1 1 0
*)
let make_decision ({ current_decision_level = d; _ } as f) =
  let l = choose_literal f in
  let dec = Assignment.Decision { literal = l; level = d + 1 } in
  make_assignment l dec f
  |> Result.map (fun f' -> { f' with current_decision_level = d + 1 })
