open Common

type formula = {
  clauses : Clause.Map.t;
  occur : Occurrence.Map.t;
  frequency : Frequency.Map.t;
  unit_clauses : (int * Clause.t) list;
  current_decision_level : int;
  assignments : Assignment.Map.t;
  trail : (Assignment.t * formula) list;
  database : Clause.t list;
  (* TODO WatchedClause set *)
  watchers : WatchedClause.t Literal.Map.t;
}

let show
    ({
       clauses;
       occur;
       frequency;
       current_decision_level;
       database;
       watchers;
       _;
     } as f) =
  let open Printf in
  sprintf
    "Clauses:\n\
     %s\n\
     Literals:\n\
     %s\n\
     Frequency:\n\
     %s\n\
     Decision level: %d\n\
     Assignments:\n\
     %s\n\
     Learned clauses:\n\
     %s\n\
     Watched literals:\n\
     %s"
    (if Clause.Map.is_empty clauses then "()" else Clause.Map.show clauses)
    (if Occurrence.Map.is_empty occur then "()" else Occurrence.Map.show occur)
    (if Frequency.Map.is_empty frequency then "()"
     else Frequency.Map.show frequency)
    current_decision_level
    (List.fold_left
       (fun acc (ass, _) -> sprintf "%s%s" acc (Assignment.show ass))
       "" f.trail)
    (List.fold_left
       (fun acc c ->
         sprintf "%s(%s)\n" acc
           (Clause.fold
              (fun l acc -> sprintf "%s%s " acc (Literal.show l))
              c ""))
       "" database)
    (* TODO pretty much the same as occurrence *)
    (Literal.Map.fold
       (fun l cs s ->
         Printf.sprintf "%s%s:%s\n" s (Literal.show l)
           (WatchedClause.fold
              (fun acc l -> Printf.sprintf "%s%s " acc (Literal.show l))
              "" cs))
       watchers "")

let add_clause
    ({
       clauses;
       occur;
       frequency;
       unit_clauses = uc;
       assignments = a;
       watchers;
       _;
     } as f) clause =
  let n = Clause.Map.size clauses in
  let clauses' = Clause.Map.add clause clauses in
  let occur' =
    Clause.fold
      (fun l m ->
        Occurrence.Map.update l
          (function
            | Some s -> Some (IntSet.add n s)
            | None -> Some (IntSet.singleton n))
          m)
      clause occur
  in
  let uc' =
    let unassigned =
      Clause.to_iter clause
      |> Iter.filter_map (fun l ->
             match Assignment.Map.find_opt l a with
             | Some _ -> None
             | None -> Some l)
      |> Clause.of_iter
    in
    if Clause.size unassigned = 1 then (n, clause) :: uc else uc
  in
  let frequency' =
    Frequency.Map.add_many
      (Clause.to_iter clause
      |> Iter.filter (fun l -> not (Assignment.Map.mem l a))
      |> Clause.of_iter)
      frequency
  in
  let watched_clause = WatchedClause.of_clause clause in
  let l1, l2 = WatchedClause.watched_literals watched_clause in
  (* TODO pretty much the same as occurrence, use update () *)
  let watchers' =
    Literal.Map.add l1 watched_clause watchers
    |> Literal.Map.add l2 watched_clause
  in
  {
    f with
    clauses = clauses';
    occur = occur';
    frequency = frequency';
    unit_clauses = uc';
    watchers = watchers';
  }

let add_learned_clauses ({ assignments = a; _ } as f) db =
  let f' =
    let open Iter in
    List.to_iter db
    |> filter_map (fun c ->
           if
             Clause.to_iter c
             |> exists (fun l ->
                    match Assignment.Map.find_opt l a with
                    | Some x ->
                        Literal.signum (Assignment.literal x) = Literal.signum l
                    | _ -> false)
           then None
           else Some c)
    |> fold add_clause f
  in
  { f' with database = db }

let analyze_conflict { current_decision_level = d; assignments = a; _ } clause =
  let ls = Clause.to_list clause in
  let rec aux q c history =
    match CCFQueue.take_front q with
    | None -> c
    | Some (l, q') -> (
        match Assignment.(Map.find_opt l a) with
        | Some (Decision _) -> aux q' (Clause.add l c) history
        | Some (Implication { implicant = ls'; level = d'; _ }) ->
            if d' < d then aux q' (Clause.add l c) history
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

let backtrack
    { current_decision_level = d; assignments = a; trail = t; database = db; _ }
    learned_clause =
  let d' =
    let open Iter in
    Clause.to_iter learned_clause
    |> filter_map (fun l -> Assignment.Map.find_opt l a)
    |> map Assignment.level
    |> filter (fun d' -> d' < d)
    |> max |> Option.value ~default:0
  in
  let _, f' =
    if d' = 0 then List.last_opt t |> Option.get_exn_or "TRAIL"
    else List.find (fun (ass, _) -> Assignment.was_decided_on_level ass d') t
  in
  let f' = { f' with frequency = Frequency.Map.decay f'.frequency } in
  let f'' = add_learned_clauses f' (learned_clause :: db) in
  (f'', d')

let check_invariants
    ({
       clauses = cm;
       occur = lm;
       (* original_clauses = oc; *)
       current_decision_level = d;
       assignments = a;
       trail = t;
       database = db;
       unit_clauses = uc;
       _;
     } as f) =
  let my_assert assertions =
    let rec aux assertions has_error =
      match assertions with
      | (c, msg) :: t ->
          if not c then Printf.printf "ASSERTION FAILED: %s\n" msg;
          aux t (has_error || not c)
      | [] -> assert (not has_error)
    in
    aux assertions false
  in
  let open Iter in
  let no_empty_clauses =
    not (Clause.Map.to_iter cm |> exists (fun (_, c) -> Clause.is_empty c))
  in
  (* let is_subset_of_original = *)
  (*   Clause.Map.to_iter cm *)
  (*   |> for_all (fun (k, v) -> *)
  (*          match Clause.Map.find_opt k oc with *)
  (*          | Some v' -> Clause.subset v v' *)
  (*          | None -> false) *)
  (* in *)
  let decision_level_non_negative = d >= 0 in
  let assignments_valid =
    Assignment.Map.to_iter a
    |> for_all (fun (k, v) ->
           Literal.equal k (Literal.var (Assignment.literal v)))
  in
  let trail_valid =
    List.to_iter t
    |> for_all (fun (x, _) ->
           let x' = Assignment.literal x in
           not
             (List.to_iter t
             |> exists (fun (y, _) ->
                    let y' = Assignment.literal y in
                    Literal.equal x' (Literal.neg y'))))
  in
  let trail_geq_decision_level = List.length t >= d in
  let clauses_literals_eq =
    Clause.Map.to_iter cm
    |> for_all (fun (c, ls) ->
           Clause.to_iter ls
           |> for_all (fun l ->
                  IntSet.to_iter (Occurrence.Map.find l lm)
                  |> for_all (fun c' -> Clause.mem l (Clause.Map.find c' cm))
                  && IntSet.mem c (Occurrence.Map.find l lm)))
  in
  let literals_clauses_eq =
    Occurrence.Map.to_iter lm
    |> for_all (fun (l, cs) ->
           IntSet.to_iter cs
           |> for_all (fun c ->
                  Clause.to_iter (Clause.Map.find c cm)
                  |> for_all (fun l' ->
                         IntSet.mem c (Occurrence.Map.find l' lm))
                  && Clause.mem l (Clause.Map.find c cm)))
  in
  let learned_clauses_no_empty_clauses =
    not (List.to_iter db |> exists (fun c -> Clause.is_empty c))
  in
  let unit_clauses_really_unit =
    List.for_all
      (fun (_, c) ->
        Clause.to_iter c
        |> filter_count (fun l -> not (Assignment.Map.mem l a))
        = 1)
      uc
  in
  my_assert
    [
      (no_empty_clauses, "Formula contains empty clause");
      (* (is_subset_of_original, "Formula has diverged from its original form"); *)
      (decision_level_non_negative, "Decision level is not non-negative");
      (assignments_valid, "Assignments are invalid");
      (trail_valid, "Trail has duplicate assignments:\n" ^ show f);
      ( trail_geq_decision_level,
        "Fewer assignments than decision levels in trail" );
      (clauses_literals_eq, "Clauses and literals out of sync");
      (literals_clauses_eq, "Literals and clauses out of sync");
      (learned_clauses_no_empty_clauses, "Learned empty clause");
      (unit_clauses_really_unit, "Unit clauses are incorrect");
    ];
  ()

(* let choose_literal { occur; frequency; _ } = *)
let choose_literal { frequency; _ } =
  (* match Frequency.Map.pop frequency with *)
  (* | None -> Occurrence.Map.choose occur |> fst *)
  (* | Some l -> l *)
  Frequency.Map.pop frequency |> Option.get_exn_or "CHOOSE"

let is_empty { frequency; _ } = Frequency.Map.is_empty frequency

let of_list _v _c =
  let rec aux f = function
    | [] -> f
    | c :: cs ->
        let clause = Clause.of_int_list c in
        let f' = add_clause f clause in
        aux f' cs
  in
  aux
    {
      clauses = Clause.Map.empty;
      occur = Occurrence.Map.empty;
      (* occur = Occurrence.Map.create v; *)
      frequency = Frequency.Map.empty;
      current_decision_level = 0;
      assignments = Assignment.Map.empty;
      (* assignments = Assignment.Map.create v; *)
      trail = [];
      database = [];
      unit_clauses = [];
      watchers = Literal.Map.empty;
    }

let restart ({ trail = t; database = db; _ } as f) =
  if List.is_empty t then f
  else
    add_learned_clauses
      (snd
         (List.last_opt t
         |> Option.get_exn_or
              "Attempt to backtrack without previous assignments."))
      db

let make_assignment l ass
    ({
       clauses;
       occur;
       frequency;
       assignments = a;
       unit_clauses = uc;
       trail = t;
       _;
     } as f) =
  let exception Conflict of Clause.t * formula in
  let a' = Assignment.Map.add l ass a in
  let t' = (ass, f) :: t in
  let f = { f with assignments = a'; trail = t' } in
  try
    let uc', f' =
      match Occurrence.Map.find_opt (Literal.neg l) occur with
      | Some cs ->
          IntSet.fold
            (fun c (uc', f') ->
              let ls = Clause.Map.find c clauses in
              let uc'', f'', unassigned, falsified, satisfied =
                Clause.fold
                  (fun l (uc'', f'', unassigned', falsified', satisfied') ->
                    match Assignment.Map.find_opt l a' with
                    | Some ass' ->
                        let l' = Assignment.literal ass' in
                        let falso = Literal.signum l <> Literal.signum l' in
                        ( uc'',
                          f'',
                          unassigned',
                          falsified' && falso,
                          satisfied' || not falso )
                    | None ->
                        (uc'', f'', Clause.add l unassigned', false, satisfied'))
                  ls
                  (uc', f', Clause.empty, true, false)
              in
              if satisfied then (uc'', f'')
              else if falsified then raise_notrace (Conflict (ls, f))
              else if Clause.size unassigned = 1 then ((c, ls) :: uc'', f'')
              else (uc'', f''))
            cs (uc, f)
      | None -> (uc, f)
    in
    let frequency' =
      Frequency.Map.remove_literal l frequency
      |> Frequency.Map.remove_literal (Literal.neg l)
    in
    let f' = { f' with frequency = frequency'; unit_clauses = uc' } in
    Ok f'
  with Conflict (c, f) -> Error (c, f)

let eliminate_pure_literals ({ occur; _ } as f) =
  let f' =
    let open Iter in
    Occurrence.Map.to_iter occur
    |> fold
         (fun ({ occur; _ } as f') (l, _) ->
           if Occurrence.Map.mem (Literal.neg l) occur then f'
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

let rec unit_propagate ({ unit_clauses = ucs; assignments = a; _ } as f) =
  match ucs with
  | (_, uc) :: ucs' -> (
      let f' = { f with unit_clauses = ucs' } in
      match
        Clause.to_iter uc
        |> Iter.find_pred (fun l -> not (Assignment.Map.mem l a))
      with
      | Some l ->
          let d' =
            let open Iter in
            Clause.remove l uc |> Clause.to_iter
            |> map (fun l ->
                   Assignment.Map.find_opt l a
                   |> Option.map_or ~default:0 Assignment.level)
            |> max |> Option.get_or ~default:0
          in
          let i =
            Assignment.Implication { literal = l; implicant = uc; level = d' }
          in
          make_assignment l i f' |> Result.flat_map unit_propagate
      | None -> unit_propagate f')
  | [] -> Ok f

let make_decision ({ current_decision_level = d; _ } as f) =
  let l = choose_literal f in
  let dec = Assignment.Decision { literal = l; level = d + 1 } in
  let f' = make_assignment l dec f |> Result.get_exn in
  { f' with current_decision_level = d + 1 }
