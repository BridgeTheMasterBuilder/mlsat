type formula = {
  clauses : Clause.Map.t;
  frequency : Frequency.Map.t;
  unit_clauses : (int * Clause.t) CCFQueue.t;
  current_decision_level : int;
  assignments : Assignment.Map.t;
  trail : (Assignment.t * formula) list;
  database : Clause.t list;
  watchers : WatchedClause.Map.t;
}

exception Conflict of Clause.t * formula

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
    (CCFQueue.fold
       (fun acc (_, c) -> Printf.sprintf "%s( %s)\n" acc (Clause.show c))
       "" unit_clauses)

let add_clause
    ({ clauses; frequency; unit_clauses = uc; assignments = a; watchers; _ } as
    f) clause =
  let n = Clause.Map.size clauses in
  let clauses' = Clause.Map.add clause clauses in
  let frequency' =
    Frequency.Map.add_many
      (Clause.to_iter clause
      |> Iter.filter (fun l -> not (Assignment.Map.mem l a))
      |> Clause.of_iter)
      frequency
  in
  let watched_clause = WatchedClause.of_clause a clause n in
  let watchers', uc' =
    match WatchedClause.watched_literals watched_clause with
    | Some (l1, l2) ->
        ( WatchedClause.Map.add l1 watched_clause watchers
          |> WatchedClause.Map.add l2 watched_clause,
          uc )
    | None -> (watchers, CCFQueue.snoc uc (n, clause))
  in
  {
    f with
    clauses = clauses';
    frequency = frequency';
    unit_clauses = uc';
    watchers = watchers';
  }

let analyze_conflict { current_decision_level = d; assignments = a; _ } clause =
  let ls = Clause.to_list clause in
  let rec aux q c history =
    match CCFQueue.take_front q with
    | None -> c
    | Some (l, q') -> (
        if CCFQueue.is_empty q' then
          let ass =
            Assignment.Map.find_opt l a |> Option.get_exn_or "ANALYZE"
          in
          Clause.add (Literal.neg (Assignment.literal ass)) c
        else
          match Assignment.(Map.find_opt l a) with
          | Some (Decision { literal = l'; _ }) ->
              aux q' (Clause.add (Literal.neg l') c) history
          | Some (Implication { literal = l'; implicant = ls'; level = d'; _ })
            ->
              if d' < d then aux q' (Clause.add (Literal.neg l') c) history
              else
                let open Iter in
                let unseen =
                  Clause.to_iter ls'
                  |> filter (fun l'' -> not Literal.(Set.mem (var l'') history))
                in
                let q'' = CCFQueue.add_iter_back q' unseen in
                let history' =
                  Literal.(Set.(union history (map var (of_iter unseen))))
                in
                aux q'' c history'
          | None -> aux q' c history)
  in
  aux (CCFQueue.of_list ls) Clause.empty
    (Literal.Set.of_list (List.map Literal.var ls))

let assignments { assignments = a; _ } = Assignment.Map.assignments a
let learned_clauses { database; _ } = database

let backtrack
    { clauses; assignments = a; trail = t; database = db; watchers; _ }
    learned_clause =
  let d' =
    let open Iter in
    Clause.to_iter learned_clause
    |> filter_map (fun l -> Assignment.Map.find_opt l a)
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
    }
  in
  let f'' = add_clause f' learned_clause in
  (f'', d')

(* let check_invariants *)
(*     ({ *)
(*        clauses = cm; *)
(*        occur = lm; *)
(*        current_decision_level = d; *)
(*        assignments = a; *)
(*        trail = t; *)
(*        database = db; *)
(*        unit_clauses = uc; *)
(*        watchers; *)
(*        _; *)
(*      } as f) = *)
(*   let my_assert assertions = *)
(*     let rec aux assertions has_error = *)
(*       match assertions with *)
(*       | (c, msg) :: t -> *)
(*           if not c then Logs.err (fun m -> m "ASSERTION FAILED: %s" msg); *)
(*           aux t (has_error || not c) *)
(*       | [] -> assert (not has_error) *)
(*     in *)
(*     aux assertions false *)
(*   in *)
(*   let open Iter in *)
(*   let no_empty_clauses = *)
(*     not (Clause.Map.to_iter cm |> exists (fun (_, c) -> Clause.is_empty c)) *)
(*   in *)
(*   (\* let is_subset_of_original = *\) *)
(*   (\*   Clause.Map.to_iter cm *\) *)
(*   (\*   |> for_all (fun (k, v) -> *\) *)
(*   (\*          match Clause.Map.find_opt k oc with *\) *)
(*   (\*          | Some v' -> Clause.subset v v' *\) *)
(*   (\*          | None -> false) *\) *)
(*   (\* in *\) *)
(*   let decision_level_non_negative = d >= 0 in *)
(*   let assignments_valid = *)
(*     Assignment.Map.to_iter a *)
(*     |> for_all (fun (k, v) -> *)
(*            Literal.equal k (Literal.var (Assignment.literal v))) *)
(*   in *)
(*   let trail_valid = *)
(*     List.to_iter t *)
(*     |> for_all (fun (x, _) -> *)
(*            let x' = Assignment.literal x in *)
(*            not *)
(*              (List.to_iter t *)
(*              |> exists (fun (y, _) -> *)
(*                     let y' = Assignment.literal y in *)
(*                     Literal.equal x' (Literal.neg y')))) *)
(*   in *)
(*   let trail_geq_decision_level = List.length t >= d in *)
(*   let clauses_literals_eq = *)
(*     Clause.Map.to_iter cm *)
(*     |> for_all (fun (c, ls) -> *)
(*            Clause.to_iter ls *)
(*            |> for_all (fun l -> *)
(*                   IntSet.to_iter (Occurrence.Map.find l lm) *)
(*                   |> for_all (fun c' -> Clause.mem l (Clause.Map.find c' cm)) *)
(*                   && IntSet.mem c (Occurrence.Map.find l lm))) *)
(*   in *)
(*   let literals_clauses_eq = *)
(*     Occurrence.Map.to_iter lm *)
(*     |> for_all (fun (l, cs) -> *)
(*            IntSet.to_iter cs *)
(*            |> for_all (fun c -> *)
(*                   Clause.to_iter (Clause.Map.find c cm) *)
(*                   |> for_all (fun l' -> *)
(*                          IntSet.mem c (Occurrence.Map.find l' lm)) *)
(*                   && Clause.mem l (Clause.Map.find c cm))) *)
(*   in *)
(*   let learned_clauses_no_empty_clauses = *)
(*     not (List.to_iter db |> exists (fun c -> Clause.is_empty c)) *)
(*   in *)
(*   let unit_clauses_really_unit = *)
(*     CCFQueue.to_iter uc *)
(*     |> Iter.for_all (fun (_, c) -> *)
(*            Clause.to_iter c *)
(*            |> filter_count (fun l -> not (Assignment.Map.mem l a)) *)
(*            = 1) *)
(*   in *)
(*   let watched_literals_are_watched = *)
(*     WatchedClause.Map.to_iter watchers *)
(*     |> for_all (fun (l, wcs) -> *)
(*            WatchedClause.Set.for_all *)
(*              (fun wc -> *)
(*                match WatchedClause.watched_literals wc with *)
(*                | None -> false *)
(*                | Some (w1, w2) -> Literal.equal l w1 || Literal.equal l w2) *)
(*              wcs) *)
(*   in *)
(*   let watched_literals_are_nonfalse = *)
(*     let nonfalse l = *)
(*       Assignment.Map.find_opt l a *)
(*       |> Option.map_or ~default:true (fun ass -> *)
(*              let l' = Assignment.literal ass in *)
(*              Literal.signum l = Literal.signum l') *)
(*     in *)
(*     WatchedClause.Map.to_iter watchers *)
(*     |> for_all (fun (_, wcs) -> *)
(*            WatchedClause.Set.for_all *)
(*              (fun wc -> *)
(*                match WatchedClause.watched_literals wc with *)
(*                | None -> true *)
(*                | Some (w1, w2) -> nonfalse w1 && nonfalse w2) *)
(*              wcs) *)
(*   in *)
(*   my_assert *)
(*     [ *)
(*       (no_empty_clauses, "Formula contains empty clause"); *)
(*       (\* (is_subset_of_original, "Formula has diverged from its original form"); *\) *)
(*       (decision_level_non_negative, "Decision level is not non-negative"); *)
(*       (assignments_valid, "Assignments are invalid"); *)
(*       (trail_valid, "Trail has duplicate assignments:\n" ^ show f); *)
(*       ( trail_geq_decision_level, *)
(*         "Fewer assignments than decision levels in trail" ); *)
(*       (\* (clauses_literals_eq, "Clauses and literals out of sync"); *\) *)
(*       (\* (literals_clauses_eq, "Literals and clauses out of sync"); *\) *)
(*       (learned_clauses_no_empty_clauses, "Learned empty clause"); *)
(*       (\* (unit_clauses_really_unit, "Unit clauses are incorrect"); *\) *)
(*       ( watched_literals_are_watched, *)
(*         "Watched literals are not correctly watched" ); *)
(*       (\* (watched_literals_are_nonfalse, "Watching false literals"); *\) *)
(*     ]; *)
(*   () *)

let choose_literal { frequency; _ } =
  Frequency.Map.pop frequency |> Option.get_exn_or "CHOOSE"

let is_empty { frequency; _ } = Frequency.Map.is_empty frequency

let of_list v c =
  let rec aux f = function
    | [] -> f
    | c :: cs ->
        let clause = Clause.of_int_list c in
        let f' = add_clause f clause in
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
      unit_clauses = CCFQueue.empty;
      watchers = WatchedClause.Map.make v;
    }

let restart ({ clauses; trail = t; watchers; database = db; _ } as f) =
  if List.is_empty t then f
  else
    let f' =
      snd
        (List.last_opt t
        |> Option.get_exn_or
             "Attempt to backtrack without previous assignments.")
    in
    { f' with clauses; watchers; database = db }

let update_watchers l ({ assignments = a; watchers; _ } as f) =
  let update_watcher l c ({ unit_clauses = uc'; watchers = watchers'; _ } as f')
      =
    let n, ls = WatchedClause.clause c in
    match WatchedClause.update l a c with
    | WatcherChange (w1, w1', w2, c') ->
        let watchers' =
          WatchedClause.Map.remove w1 c watchers'
          (* |> WatchedClause.Map.remove w2 c (\* TODO *\) *)
          |> WatchedClause.Map.add w1' c'
          |> WatchedClause.Map.add w2 c'
        in
        { f' with watchers = watchers' }
    | Unit _ -> { f' with unit_clauses = CCFQueue.snoc uc' (n, ls) }
    | Falsified _ -> raise_notrace (Conflict (ls, f))
    | NoChange -> f'
  in
  match WatchedClause.Map.find_opt l watchers with
  | Some cs -> WatchedClause.Set.fold (fun c f' -> update_watcher l c f') cs f
  | None -> f

let make_assignment l ass ({ frequency; assignments = a; trail = t; _ } as f) =
  let a' = Assignment.Map.add l ass a in
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
  match CCFQueue.take_front ucs with
  | Some ((_, uc), ucs') -> (
      let f' = { f with unit_clauses = ucs' } in
      match
        Clause.to_iter uc
        |> Iter.find_pred (fun l -> not (Assignment.Map.mem l a))
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
