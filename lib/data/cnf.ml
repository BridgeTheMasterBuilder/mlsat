open Common

type formula = {
  clauses : ClauseMap.t;
  occur : OccurrenceMap.t;
  frequency : FrequencyMap.t;
  original_clauses : ClauseMap.t;
  unit_clauses : ClauseMap.t;
  current_decision_level : int;
  assignments : Assignment.Map.t;
  trail : (Assignment.t * formula) list;
  database : Clause.t list;
}

let show
    ({ clauses; occur; frequency; current_decision_level; database; _ } as f) =
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
     %s"
    (if ClauseMap.is_empty clauses then "()" else ClauseMap.show clauses)
    (if OccurrenceMap.is_empty occur then "()" else OccurrenceMap.show occur)
    (if FrequencyMap.is_empty frequency then "()"
     else FrequencyMap.show frequency)
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

let add_clause
    ({ clauses; occur; frequency; original_clauses = oc; unit_clauses = uc; _ }
    as f) clause original_clause =
  let n = ClauseMap.size oc in
  let clauses' = ClauseMap.add (n + 1) clause clauses in
  let occur' =
    Clause.fold
      (fun l m ->
        OccurrenceMap.update l
          (function
            | Some s -> Some (IntSet.add (n + 1) s)
            | None -> Some (IntSet.singleton (n + 1)))
          m)
      clause occur
  in
  let oc' = ClauseMap.add (n + 1) original_clause oc in
  let uc' =
    if Clause.size clause = 1 then ClauseMap.add (n + 1) clause uc else uc
  in
  let frequency' = FrequencyMap.add_many clause frequency in
  {
    f with
    clauses = clauses';
    occur = occur';
    frequency = frequency';
    original_clauses = oc';
    unit_clauses = uc';
  }

let of_list =
  let rec aux f n = function
    | [] -> f
    | c :: cs ->
        let clause = Clause.of_list c in
        let f' = add_clause f clause clause in
        aux f' (n + 1) cs
  in
  aux
    {
      clauses = ClauseMap.empty;
      occur = OccurrenceMap.empty;
      frequency = FrequencyMap.empty;
      original_clauses = ClauseMap.empty;
      current_decision_level = 0;
      assignments = Literal.Map.empty;
      trail = [];
      database = [];
      unit_clauses = ClauseMap.empty;
    }
    1

let is_empty { clauses; _ } = ClauseMap.is_empty clauses

let choose_literal { occur; frequency; _ } =
  match FrequencyMap.pop frequency with
  | None -> OccurrenceMap.choose occur |> fst
  | Some ((l, _), _) -> l

let delete_literal ({ occur; frequency; original_clauses = oc; _ } as f) l =
  let exception Conflict of Clause.t * formula in
  match OccurrenceMap.find_opt l occur with
  | None -> Ok f
  | Some cs -> (
      try
        let f' =
          IntSet.fold
            (fun c f' ->
              match f' with
              | { clauses; unit_clauses = uc; _ } as f'' -> (
                  match ClauseMap.find_opt c clauses with
                  | None -> f''
                  | Some ls -> (
                      let diff = Clause.remove l ls in
                      match Clause.size diff with
                      | 0 -> raise_notrace (Conflict (ClauseMap.find c oc, f))
                      | 1 ->
                          {
                            f'' with
                            clauses = ClauseMap.add c diff clauses;
                            unit_clauses = ClauseMap.add c diff uc;
                          }
                      | 2 -> { f'' with clauses = ClauseMap.add c diff clauses }
                      | _ -> { f'' with clauses = ClauseMap.add c diff clauses }
                      )))
            cs f
        in
        let occur' = OccurrenceMap.remove l occur in
        let frequency' = FrequencyMap.remove_literal l frequency in
        Ok { f' with occur = occur'; frequency = frequency' }
      with Conflict (c, f) -> Error (c, f))

let delete_clauses f cs =
  IntSet.fold
    (fun c ({ clauses; occur; frequency; unit_clauses = uc; _ } as f') ->
      let ls = ClauseMap.find c clauses in
      let occur', frequency' =
        Clause.fold
          (fun l (occur', frequency') ->
            let diff = IntSet.remove c (OccurrenceMap.find l occur') in
            if IntSet.is_empty diff then
              ( OccurrenceMap.remove l occur',
                FrequencyMap.remove_literal l frequency' )
            else
              ( OccurrenceMap.add l diff occur',
                (* FrequencyMap.remove_clause l frequency' *) frequency' ))
          ls (occur, frequency)
      in
      let clauses' = ClauseMap.remove c clauses in
      let uc' = ClauseMap.remove c uc in
      {
        f' with
        clauses = clauses';
        occur = occur';
        frequency = frequency';
        unit_clauses = uc';
      })
    cs f

let simplify ({ occur; _ } as f) l =
  let raw_delete_literal ({ occur; _ } as f) l =
    { f with occur = OccurrenceMap.remove l occur }
  in
  delete_literal f (Literal.neg l)
  |> Result.map (fun f' ->
         match OccurrenceMap.find_opt l occur with
         | None -> f'
         | Some cs -> raw_delete_literal (delete_clauses f' cs) l)

let rec unit_propagate
    ({ original_clauses = oc; unit_clauses = uc; assignments = a; trail = t; _ }
    as f) =
  match ClauseMap.choose_opt uc with
  | Some (c, ls) ->
      let l' = Clause.choose ls in
      let ls' = ClauseMap.find c oc in
      let d' =
        let open Iter in
        Clause.remove l' ls' |> Clause.to_iter
        |> map (fun l ->
               Assignment.Map.find_opt l a
               |> Option.map_or ~default:0 Assignment.level)
        |> max |> Option.get_or ~default:0
      in
      let i =
        Assignment.Implication { literal = l'; implicant = ls'; level = d' }
      in
      let a' = Assignment.Map.add l' i a in
      let t' = (i, f) :: t in
      let f' = { f with assignments = a'; trail = t' } in
      simplify f' l' |> Result.flat_map unit_propagate
  | None -> Ok f

let rewrite ({ current_decision_level = d; assignments = a; trail = t; _ } as f)
    l =
  let f' = Result.get_exn (simplify f l) in
  let dec = Assignment.Decision { literal = l; level = d + 1 } in
  let a' = Assignment.Map.add l dec a in
  let t' = (dec, f) :: t in
  { f' with current_decision_level = d + 1; assignments = a'; trail = t' }

let analyze_conflict { current_decision_level = d; assignments = a; _ } clause =
  let ls = Clause.to_list clause in
  let rec aux q c history =
    match CCFQueue.take_front q with
    | None -> c
    | Some (l, q') -> (
        (* if CCFQueue.is_empty q' then Clause.add l c *)
        (* else *)
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
        | _ -> aux q' c history)
  in
  aux (CCFQueue.of_list ls) Clause.empty
    (Literal.Set.of_list (List.map Literal.var ls))

let add_learned_clauses ({ assignments = a; _ } as f) db =
  let open Iter in
  let f' =
    List.to_iter db
    |> map (fun c ->
           ( Clause.fold
               (fun l c' ->
                 match Assignment.Map.find_opt l a with
                 | Some x ->
                     if
                       Literal.signum (Assignment.literal x) <> Literal.signum l
                     then Option.map (fun c' -> Clause.remove l c') c'
                     else None
                 | _ -> c')
               c (Some c),
             c ))
    |> filter_map (fun (c, oc) -> Option.map (fun c -> (c, oc)) c)
    |> fold (fun f' (c, oc) -> add_clause f' c oc) f
  in
  { f' with database = db }

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
  let f' = { f' with frequency = FrequencyMap.decay f'.frequency } in
  let f'' = add_learned_clauses f' (learned_clause :: db) in
  (f'', d')

let restart ({ trail = t; database = db; _ } as f) =
  if List.is_empty t then f
  else
    add_learned_clauses
      (snd
         (List.last_opt t
         |> Option.get_exn_or
              "Attempt to backtrack without previous assignments."))
      db

let my_assert assertions =
  let rec aux assertions has_error =
    match assertions with
    | (c, msg) :: t ->
        if not c then Printf.printf "ASSERTION FAILED: %s\n" msg;
        aux t (has_error || not c)
    | [] -> assert (not has_error)
  in
  aux assertions false

let check_invariants
    ({
       clauses = cm;
       occur = lm;
       (* original_clauses = oc; *)
       current_decision_level = d;
       assignments = a;
       trail = t;
       database = db;
       _;
     } as f) =
  let open Iter in
  let no_empty_clauses =
    not (ClauseMap.to_iter cm |> exists (fun (_, c) -> Clause.is_empty c))
  in
  (* let is_subset_of_original = *)
  (*   ClauseMap.to_iter cm *)
  (*   |> for_all (fun (k, v) -> *)
  (*          match ClauseMap.find_opt k oc with *)
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
    ClauseMap.to_iter cm
    |> for_all (fun (c, ls) ->
           Clause.to_iter ls
           |> for_all (fun l ->
                  IntSet.to_iter (OccurrenceMap.find l lm)
                  |> for_all (fun c' -> Clause.mem l (ClauseMap.find c' cm))
                  && IntSet.mem c (OccurrenceMap.find l lm)))
  in
  let literals_clauses_eq =
    OccurrenceMap.to_iter lm
    |> for_all (fun (l, cs) ->
           IntSet.to_iter cs
           |> for_all (fun c ->
                  Clause.to_iter (ClauseMap.find c cm)
                  |> for_all (fun l' -> IntSet.mem c (OccurrenceMap.find l' lm))
                  && Clause.mem l (ClauseMap.find c cm)))
  in
  let learned_clauses_no_empty_clauses =
    not (List.to_iter db |> exists (fun c -> Clause.is_empty c))
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
    ];
  ()
