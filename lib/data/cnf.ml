type formula = {
  _debug_clauses : int list list;
  frequency : Frequency.Map.t;
  current_decision_level : int;
  assignments : Assignment.Map.t;
  trail : (Assignment.t * formula) list;
  (* TODO rename to trace and include clause deletion too *)
  database : Clause.t list;
  watchers : Clause.Watched.Map.t;
  unwatched : Clause.Set.t;
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
    ({ frequency; assignments = a; trail = t; _ } as f) d ucs =
  let update_watchers l ({ assignments = a; watchers; _ } as f) ucs' =
    let update_watcher l c ({ watchers = watchers'; unwatched; _ } as f') ucs''
        =
      match Clause.Watched.update l a c watchers' with
      | WatchedLiteralChange watchers' ->
          Ok
            ( {
                f' with
                watchers = watchers';
                (* TODO It might already not be watched, need a way to figure out when this is necessary, or maybe it doesn't matter? *)
                unwatched =
                  Clause.Set.remove (Clause.Watched.to_clause c) unwatched;
              },
              ucs'' )
      | Unit (l, clause) ->
          Logs.debug (fun m ->
              m "Unit propagating %s at level %d because of %s" (Literal.show l)
                f'.current_decision_level (Clause.show clause));
          Ok (f', (l, clause) :: ucs'')
      | Falsified clause ->
          (* Logs.debug (fun m -> *)
          (*     m "(%d) Clause %s is falsified" f.current_decision_level *)
          (*       (Clause.show clause)); *)
          Error (clause, f)
      | NoChange -> Ok (f', ucs'')
    in
    (* TODO Pretty gnarly code *)
    match Clause.Watched.Map.find_opt l watchers with
    | Some cs -> (
        let f', ucs'' =
          try
            Pair.map_fst Result.return
              (Clause.Watched.Set.fold
                 (fun c (f'', ucs'') ->
                   update_watcher l c f'' ucs''
                   |> Result.get_lazy (fun (c, f) ->
                          raise_notrace (Conflict (c, f))))
                 cs (f, ucs'))
          with Conflict (c, f) -> (Error (c, f), [])
        in
        match f' with
        | Ok f' -> (unit_propagate [@tailcall]) f' ucs''
        | error -> error)
    | None -> (unit_propagate [@tailcall]) f ucs'
  in
  let a' = Assignment.Map.add (Literal.var l) ass a in
  let t' = (ass, f) :: t in
  let f = { f with assignments = a'; trail = t'; current_decision_level = d } in
  Logs.debug (fun m -> m "Making assignment %s" (Assignment.show ass));
  let frequency' =
    Frequency.Map.remove_literal l frequency
    |> Frequency.Map.remove_literal (Literal.neg l)
  in
  let f = { f with frequency = frequency' } in
  update_watchers (Literal.neg l) f ucs

and unit_propagate ({ current_decision_level = d; _ } as f) ucs =
  (* Logs.debug (fun m -> m "Unit clauses:"); *)
  (* Logs.debug (fun m -> *)
  (*     m "%s" *)
  (*       (List.fold_left *)
  (*          (fun s (_, c) -> Printf.sprintf "%s( %s) " s (Clause.show c)) *)
  (*          "" ucs)); *)
  match ucs with
  | [] -> Ok f
  | (l, uc) :: ucs' ->
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
  let f' = { f with frequency = frequency' } in
  match Clause.Watched.watch_clause a clause watchers with
  | WatchedLiteralChange watchers' ->
      Ok
        {
          f' with
          watchers = watchers';
          unwatched = Clause.Set.remove clause unwatched;
        }
  | Unit (l, clause) ->
      (* Array.iter *)
      (*   (fun l -> *)
      (*     Logs.debug (fun m -> *)
      (*         m "%s(%s) " (Literal.show l) *)
      (*           (Assignment.Map.find_opt (Literal.var l) a *)
      (*           |> Option.map_or ~default:"_" (fun ass -> *)
      (*                  Literal.show (Assignment.literal ass))))) *)
      (*   (Clause.to_array clause); *)
      Logs.debug (fun m ->
          m "Unit propagating %s at level %d because of %s" (Literal.show l)
            f'.current_decision_level (Clause.show clause));
      let f' = { f' with unwatched = Clause.Set.add clause unwatched } in
      unit_propagate f' [ (l, clause) ]
  | Falsified clause ->
      (* Logs.debug (fun m -> m "Clause %s is falsified" (Clause.show clause)); *)
      Error (clause, f)
  | NoChange -> Ok f'

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
      frequency;
      unwatched;
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
  (* Logs.debug (fun m -> m "Backtracking to level %d" d'); *)
  let _, f =
    if d' = 0 then List.last_opt t |> Option.get_exn_or "TRAIL"
    else List.find (fun (ass, _) -> Assignment.was_decided_on_level d' ass) t
  in
  let frequency' = Frequency.Map.merge frequency f.frequency in
  let f =
    {
      f with
      frequency = Frequency.Map.decay frequency';
      watchers;
      database = learned_clause :: db;
    }
  in
  let f' =
    Clause.Set.fold
      (fun clause f' -> add_clause clause f' |> Result.get_exn)
      unwatched f
  in
  add_clause learned_clause f' |> Result.map (fun f'' -> (f'', d'))

let choose_literal { frequency; _ } = Frequency.Map.pop frequency
let is_empty { frequency; _ } = Frequency.Map.is_empty frequency

let of_list v _c list =
  let rec aux f = function
    | [] -> f
    | c :: cs ->
        assert (List.to_iter c |> Iter.for_all (fun l -> l <> 0));
        let clause = Literal.List.of_int_list c |> Clause.of_list in
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
           unwatched = Clause.Set.empty;
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
    let f = { f with watchers; database; frequency = frequency'' } in
    Clause.Set.fold
      (* TODO *)
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
           make_assignment l i f' 0 [] |> Result.get_exn)
       f

let preprocess f =
  let f = eliminate_pure_literals f in
  { f with trail = [] }

let make_decision ({ current_decision_level = d; _ } as f) =
  let l = choose_literal f in
  let dec = Assignment.Decision { literal = l; level = d + 1 } in
  make_assignment l dec f (d + 1) []

let check ({ database = db; trail; _ } as f) =
  Logs.debug (fun m -> m "%s" (show f));
  let db_equal =
    List.sort_uniq
      ~cmp:(fun c1 c2 ->
        Array.compare Literal.compare
          (Array.sorted Literal.compare (Clause.to_array c1))
          (Array.sorted Literal.compare (Clause.to_array c2)))
      db
    |> List.length = List.length db
  in
  if not db_equal then
    List.iter
      (fun (c1, c2) ->
        Logs.debug (fun m -> m "%s = %s" (Clause.show c1) (Clause.show c2)))
      (List.combine_shortest
         (List.sort_uniq
            ~cmp:(fun c1 c2 ->
              Array.compare Literal.compare
                (Array.sorted Literal.compare (Clause.to_array c1))
                (Array.sorted Literal.compare (Clause.to_array c2)))
            db)
         db);
  assert db_equal;
  let trail_equal =
    List.sort_uniq
      ~cmp:(fun (ass1, _) (ass2, _) -> Assignment.compare ass1 ass2)
      trail
    |> List.length = List.length trail
  in
  if not trail_equal then
    List.iter
      (fun ((ass1, _), (ass2, _)) ->
        Logs.debug (fun m ->
            m "%s = %s" (Assignment.show ass1) (Assignment.show ass2)))
      (List.combine_shortest
         (List.sort_uniq
            ~cmp:(fun (ass1, _) (ass2, _) -> Assignment.compare ass1 ass2)
            trail)
         trail);
  assert trail_equal
