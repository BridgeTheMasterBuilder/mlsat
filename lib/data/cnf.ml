type formula = {
  _debug_clauses : int list list;
  frequency : Frequency.Map.t;
  current_decision_level : int;
  assignments : Assignment.Map.t;
  trail : (Assignment.t * formula) list;
  (* TODO rename to trace and include clause deletion too *)
  database : Clause.t list;
  watchers : Clause.Watched.Map.t;
}

exception Conflict of Clause.t * formula

module VariableWorkqueue : Workqueue.S with type elt = Variable.t =
  Workqueue.Make (Variable)

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
       (fun acc c -> Printf.sprintf "%s( %s)\n" acc (Clause.show c))
       "" database)
    (if Clause.Watched.Map.is_empty watchers then "()"
     else Clause.Watched.Map.show watchers)

let decision_level { current_decision_level = d; _ } = d

let rec make_assignment l ass
    ({ frequency; assignments = a; trail = t; _ } as f) d =
  let update_watchers l ({ assignments = a; watchers; _ } as f) =
    let update_watcher l c ({ watchers = watchers'; _ } as f') ucs' =
      match Clause.Watched.update l a c watchers' with
      | WatchedLiteralChange watchers' ->
          Ok ({ f' with watchers = watchers' }, ucs')
      | Unit (l, uc) ->
          Logs.debug (fun m ->
              m "Unit propagating %s because of %s" (Literal.show l)
                (Clause.show uc));
          Ok (f', (l, uc) :: ucs')
      | Falsified ls ->
          Logs.debug (fun m ->
              m "(%d) Clause %s is falsified" f.current_decision_level
                (Clause.show ls));
          Error (ls, f)
      | NoChange -> Ok (f', ucs')
    in
    try
      Ok
        (match Clause.Watched.Map.find_opt l watchers with
        | Some cs ->
            let f'', ucs =
              Clause.Watched.Set.fold
                (fun c (f', ucs) ->
                  update_watcher l c f' ucs
                  |> Result.get_lazy (fun (c, f) ->
                         raise_notrace (Conflict (c, f))))
                cs (f, [])
            in
            List.fold_left
              (fun f' (l, uc) ->
                unit_propagate (l, uc) f'
                |> Result.get_lazy (fun (c, f) ->
                       raise_notrace (Conflict (c, f))))
              f'' ucs
        | None -> f)
    with Conflict (c, f) -> Error (c, f)
  in
  let a' = Assignment.Map.add (Literal.var l) ass a in
  let t' = (ass, f) :: t in
  let f = { f with assignments = a'; trail = t'; current_decision_level = d } in
  Logs.debug (fun m -> m "Making assignment %s" (Assignment.show ass));
  let frequency' =
    Frequency.Map.remove_literal l frequency
    |> Frequency.Map.remove_literal (Literal.neg l)
  in
  let f' = { f with frequency = frequency' } in
  update_watchers (Literal.neg l) f'

and unit_propagate (l, uc) ({ current_decision_level = d; _ } as f) =
  let f' = f in
  let i =
    Assignment.Implication
      { literal = l; implicant = Clause.to_array uc; level = d }
  in
  make_assignment l i f' d

let add_clause clause ({ frequency; assignments = a; watchers; _ } as f) =
  let frequency' =
    let open Iter in
    Frequency.Map.incr_iter
      (Clause.to_iter clause
      |> filter (fun l -> not (Assignment.Map.mem (Literal.var l) a)))
      frequency
  in
  let f' = { f with frequency = frequency' } in
  match Clause.Watched.watch_clause a clause watchers with
  | WatchedLiteralChange watchers' -> Ok { f' with watchers = watchers' }
  | Unit (l, clause) ->
      Array.iter
        (fun l ->
          Logs.debug (fun m ->
              m "%s(%s) " (Literal.show l)
                (Assignment.Map.find_opt (Literal.var l) a
                |> Option.map_or ~default:"_" (fun ass ->
                       Literal.show (Assignment.literal ass)))))
        (Clause.to_array clause);
      Logs.debug (fun m ->
          m "Unit propagating %s because of %s" (Literal.show l)
            (Clause.show clause));
      unit_propagate (l, clause) f'
  | Falsified clause ->
      Logs.debug (fun m -> m "Clause %s is falsified" (Clause.show clause));
      Error (clause, f)
  | NoChange -> Ok f'

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
  (* TODO Is destructively sorting the clause here safe? *)
  (* let clause = Clause.to_array clause in *)
  (* Array.sort *)
  (*   (fun l1 l2 -> *)
  (*     let ass1 = Assignment.Map.find (Literal.var l1) a in *)
  (*     let ass2 = Assignment.Map.find (Literal.var l2) a in *)
  (*     match (ass1, ass2) with *)
  (*     | Decision { level = d1; _ }, Implication { level = d2; _ } when d1 = d2 *)
  (*       -> *)
  (*         1 *)
  (*     | Implication { level = d1; _ }, Decision { level = d2; _ } when d1 = d2 *)
  (*       -> *)
  (*         -1 *)
  (*     | Decision { level = d1; _ }, Decision { level = d2; _ } *)
  (*     | Decision { level = d1; _ }, Implication { level = d2; _ } *)
  (*     | Implication { level = d1; _ }, Decision { level = d2; _ } *)
  (*     | Implication { level = d1; _ }, Implication { level = d2; _ } -> *)
  (*         -Int.compare d1 d2) *)
  (*   clause; *)
  (* let clause = Clause.of_array clause in *)
  let clause =
    Clause.to_array clause
    |> Array.sorted (fun l1 l2 ->
           let ass1 = Assignment.Map.find (Literal.var l1) a in
           let ass2 = Assignment.Map.find (Literal.var l2) a in
           match (ass1, ass2) with
           | Decision { level = d1; _ }, Implication { level = d2; _ }
             when d1 = d2 ->
               1
           | Implication { level = d1; _ }, Decision { level = d2; _ }
             when d1 = d2 ->
               -1
           | Decision { level = d1; _ }, Decision { level = d2; _ }
           | Decision { level = d1; _ }, Implication { level = d2; _ }
           | Implication { level = d1; _ }, Decision { level = d2; _ }
           | Implication { level = d1; _ }, Implication { level = d2; _ } ->
               -Int.compare d1 d2)
    |> Clause.of_array
  in
  aux (VariableWorkqueue.of_iter (Clause.to_iter clause |> map Literal.var)) []
  |> Clause.of_list

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

(* TODO need to handle unit clauses when backtracking, they either need to be propagated or watched (seeing as they aren't watched and so won't be automatically propagated) *)
let backtrack
    { assignments = a; trail = t; database = db; watchers; frequency; _ }
    learned_clause =
  let d' =
    let open Iter in
    Clause.to_iter learned_clause
    |> filter_map (fun l -> Assignment.Map.find_opt (Literal.var l) a)
    |> map Assignment.level
    |> sort ~cmp:(fun d1 d2 -> -compare d1 d2)
    |> drop 1 |> max |> Option.value ~default:0
  in
  Logs.debug (fun m -> m "Backtracking to level %d" d');
  let _, f =
    if d' = 0 then List.last_opt t |> Option.get_exn_or "TRAIL"
    else List.find (fun (ass, _) -> Assignment.was_decided_on_level ass d') t
  in
  let frequency' = Frequency.Map.merge frequency f.frequency in
  let f' =
    {
      f with
      frequency = Frequency.Map.decay frequency';
      watchers;
      database = learned_clause :: db;
    }
  in
  (* Logs.debug (fun m -> m "Backtracking"); *)
  (* List.iter *)
  (*   (fun c -> *)
  (*     Logs.debug (fun m -> *)
  (*         m "Is clause %s watched? %b" (Clause.show c) *)
  (*           (Clause.to_iter c *)
  (*           |> Iter.exists (fun l -> *)
  (*                  Clause.Watched.Map.find_opt l f'.watchers |> Option.is_some) *)
  (*           ))) *)
  (*   f'.database; *)
  add_clause learned_clause f' |> Result.map (fun f'' -> (f'', d'))

let choose_literal { frequency; _ } = Frequency.Map.pop frequency
let is_empty { frequency; _ } = Frequency.Map.is_empty frequency

let of_list v _c list =
  let rec aux f = function
    | [] -> f
    | c :: cs ->
        assert (List.to_iter c |> Iter.for_all (fun l -> l <> 0));
        let clause = Clause.of_list (Literal.List.of_int_list c) in
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
           frequency = Frequency.Map.empty;
           current_decision_level = 0;
           assignments = Assignment.Map.empty;
           trail = [];
           database = [];
           watchers = Clause.Watched.Map.make v;
         }
         list)
  with Conflict _ -> None

let restart ({ trail = t; watchers; database = db; _ } as f) =
  if List.is_empty t then f
  else
    let f' =
      snd
        (List.last_opt t
        |> Option.get_exn_or
             "Attempt to backtrack without previous assignments.")
    in
    let frequency'' = Frequency.Map.merge f.frequency f'.frequency in
    { f' with watchers; database = db; frequency = frequency'' }

let eliminate_pure_literals ({ frequency; _ } as f) =
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
           make_assignment l i f' 0 |> Result.get_exn)
       f

let preprocess f =
  let f' = eliminate_pure_literals f in
  { f' with trail = [] }

let make_decision ({ current_decision_level = d; _ } as f) =
  let l = choose_literal f in
  let dec = Assignment.Decision { literal = l; level = d + 1 } in
  make_assignment l dec f (d + 1)

let check ({ database = db; _ } as f) =
  Logs.debug (fun m -> m "%s" (show f));
  assert (
    List.for_all
      (fun c1 ->
        not
          (List.exists
             (fun c2 ->
               let mtch =
                 Array.equal Literal.equal
                   (Array.sorted Literal.compare (Clause.to_array c1))
                   (Array.sorted Literal.compare (Clause.to_array c2))
               in
               if mtch then (
                 Logs.debug (fun m ->
                     m "Found duplicate %s = %s" (Clause.show c1)
                       (Clause.show c2));
                 mtch)
               else mtch)
             (List.remove_one
                ~eq:(fun c1 c2 ->
                  Array.equal Literal.equal (Clause.to_array c1)
                    (Clause.to_array c2))
                c1 db)))
      db)
