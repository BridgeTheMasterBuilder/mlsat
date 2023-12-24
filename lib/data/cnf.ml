open Common

type formula = {
  clauses : ClauseMap.t;
  occur : OccurrenceMap.t;
  frequency : FrequencyMap.t;
  original_clauses : ClauseMap.t;
  unit_clauses : (int * Clause.t) list;
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
    ({
       clauses;
       occur;
       frequency;
       original_clauses = oc;
       unit_clauses = uc;
       assignments = a;
       _;
     } as f) clause original_clause =
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
    (* TODO for some reason, adding unit clauses only works after an assignment *)
    if Clause.size clause = 1 then (
      Printf.printf "(FXX) Adding unit clause to queue: %s\n"
        (Clause.show clause);
      (n + 1, clause) :: uc)
    else uc
    (* (\* Printf.printf "Assignments:\n%s\n" *\) *)
    (* (\*   (List.fold_left *\) *)
    (* (\*      (fun acc (ass, _) -> Printf.sprintf "%s%s" acc (Assignment.show ass)) *\) *)
    (* (\*      "" f.trail); *\) *)
    (* let unassigned = *)
    (*   Clause.to_iter clause *)
    (*   (\* |> Iter.filter_count (fun l -> not (Assignment.Map.mem l a)) *\) *)
    (*   |> Iter.filter_map (fun l -> *)
    (*          match Assignment.Map.find_opt l a with *)
    (*          | Some _ -> None *)
    (*          | None -> Some l) *)
    (*   |> Clause.of_iter *)
    (* in *)
    (* (\* Printf.printf "%d unassigned literals in: %s\n" unassigned *\) *)
    (* (\*   (Clause.show clause); *\) *)
    (* if Clause.size unassigned = 1 then ( *)
    (*   if *)
    (*     not *)
    (*       (List.mem *)
    (*          ~eq:(fun (_, c1) (_, c2) -> Clause.equal c1 c2) *)
    (*          (n + 1, clause) *)
    (*          uc) *)
    (*   then *)
    (*     Printf.printf "(FXX) Adding unit clause to queue: %s\n" *)
    (*       (Clause.show clause); *)
    (*   List.add_nodup *)
    (*     ~eq:(fun (_, c1) (_, c2) -> Clause.equal c1 c2) *)
    (*     (n + 1, clause) *)
    (*     uc) *)
    (* else uc *)
  in
  let frequency' =
    FrequencyMap.add_many
      (Clause.to_iter clause
      |> Iter.filter (fun l -> not (Assignment.Map.mem l a))
      |> Clause.of_iter)
      frequency
  in
  {
    f with
    clauses = clauses';
    occur = occur';
    frequency = frequency';
    original_clauses = oc';
    unit_clauses = uc';
  }

(* TODO error is likely here *)
let add_learned_clauses ({ assignments = a; _ } as f) db =
  let f' = List.fold_left (fun f' c -> add_clause f' c c) f db in
  (* let open Iter in *)
  (* let f' = *)
  (*   List.to_iter db *)
  (*   |> map (fun c -> *)
  (*          ( Clause.fold *)
  (*              (fun l c' -> *)
  (*                match Assignment.Map.find_opt l a with *)
  (*                | Some x -> *)
  (*                    if *)
  (*                      Literal.signum (Assignment.literal x) <> Literal.signum l *)
  (*                    then Option.map (fun c' -> Clause.remove l c') c' *)
  (*                    else None *)
  (*                | _ -> c') *)
  (*              c (Some c), *)
  (*            c )) *)
  (*   |> filter_map (fun (c, oc) -> Option.map (fun c -> (c, oc)) c) *)
  (*   (\* |> fold (fun f' (c, oc) -> add_clause f' c oc) f *\) *)
  (*   |> fold (fun f' (c, oc) -> add_clause f' oc oc) f *)
  (* in *)
  { f' with database = db }

(* TODO examine every detail *)
let analyze_conflict { current_decision_level = d; assignments = a; _ } clause =
  let ls = Clause.to_list clause in
  (* Printf.printf "Analyzing clause: %s\n" (Clause.show clause); *)
  let rec aux q c history =
    (* print_string "State of queue: "; *)
    (* CCFQueue.iter (fun l -> print_string (Literal.show l ^ " ")) q; *)
    (* print_newline (); *)
    match CCFQueue.take_front q with
    | None -> c
    | Some (l, q') -> (
        (* Printf.printf "Examining literal %s\n" (Literal.show l); *)
        (* if CCFQueue.is_empty q' then Clause.add l c *)
        (* else *)
        match Assignment.(Map.find_opt l a) with
        (* | Some (Decision _) -> *)
        | Some (Decision { level = d'; _ }) ->
            (* Printf.printf "Adding decision literal %s (level %d) to clause" *)
            (*   (Literal.show l) d'; *)
            aux q' (Clause.add l c) history
        | Some (Implication { implicant = ls'; level = d'; _ }) ->
            (* Printf.printf "Examining implied literal %s (level %d)" *)
            (*   (Literal.show l) d'; *)
            if d' < d then
              (* Printf.printf "Adding implied literal %s to clause" *)
              (*   (Literal.show l); *)
              aux q' (Clause.add l c) history
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
    (* Printf.printf "Calculating backtrack level from %s\n" *)
    (*   (Clause.show learned_clause); *)
    Clause.to_iter learned_clause
    |> filter_map (fun l ->
           (* Printf.printf "Examining literal %s\n" (Literal.show l); *)
           Assignment.Map.find_opt l a)
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
  (* Printf.printf "Backtracking to level %d\n" d'; *)
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

let choose_literal { occur; frequency; _ } =
  match FrequencyMap.pop frequency with
  | None -> OccurrenceMap.choose occur |> fst
  | Some l ->
      Printf.printf "Deciding %s\n" (Literal.show l);
      l

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
      let uc' = List.remove_assq c uc in
      {
        f' with
        clauses = clauses';
        occur = occur';
        frequency = frequency';
        unit_clauses = uc';
      })
    cs f

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
                      | 0 ->
                          Printf.printf "Falsified clause: %s\n"
                            (Clause.show (ClauseMap.find c oc));
                          raise_notrace (Conflict (ClauseMap.find c oc, f))
                      | 1 ->
                          Printf.printf "Adding unit clause to queue: %s\n"
                            (Clause.show (ClauseMap.find c oc));
                          {
                            f'' with
                            clauses = ClauseMap.add c diff clauses;
                            unit_clauses = (c, diff) :: uc;
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

(* let is_empty { clauses; _ } = ClauseMap.is_empty clauses *)
let is_empty { frequency; _ } = FrequencyMap.is_empty frequency

let of_list =
  let rec aux f n = function
    | [] -> f
    | c :: cs ->
        let clause = Clause.of_int_list c in
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
      assignments = Assignment.Map.empty;
      trail = [];
      database = [];
      unit_clauses = [];
    }
    1

let eliminate_pure_literals ({ occur; _ } as f) =
  let open Iter in
  OccurrenceMap.to_iter occur
  |> fold
       (fun ({ clauses; occur; _ } as f') (l, cs) ->
         if OccurrenceMap.mem (Literal.neg l) occur then f'
         else
           let cs' = IntSet.filter (fun c -> ClauseMap.mem c clauses) cs in
           let ({ occur; _ } as f') = delete_clauses f' cs' in
           { f' with occur = OccurrenceMap.remove l occur })
       f

let preprocess = eliminate_pure_literals

let restart ({ trail = t; database = db; _ } as f) =
  if List.is_empty t then f
  else
    add_learned_clauses
      (snd
         (List.last_opt t
         |> Option.get_exn_or
              "Attempt to backtrack without previous assignments."))
      db

let simplify ({ occur; _ } as f) l =
  let raw_delete_literal ({ occur; _ } as f) l =
    { f with occur = OccurrenceMap.remove l occur }
  in
  delete_literal f (Literal.neg l)
  |> Result.map (fun f' ->
         match OccurrenceMap.find_opt l occur with
         | None -> f'
         | Some cs -> raw_delete_literal (delete_clauses f' cs) l)

let rewrite ({ current_decision_level = d; assignments = a; trail = t; _ } as f)
    l =
  let f' = Result.get_exn (simplify f l) in
  let dec = Assignment.Decision { literal = l; level = d + 1 } in
  let a' = Assignment.Map.add l dec a in
  let t' = (dec, f) :: t in
  { f' with current_decision_level = d + 1; assignments = a'; trail = t' }

let rec unit_propagate
    ({ original_clauses = oc; unit_clauses = uc; assignments = a; trail = t; _ }
    as f) =
  match List.head_opt uc with
  | Some (c, ls) ->
      let l = Clause.choose ls in
      let ls' = ClauseMap.find c oc in
      Printf.printf "Unit propagating by literal %s taken from %s\n"
        (Literal.show l) (Clause.show ls');
      let d' =
        let open Iter in
        Clause.remove l ls' |> Clause.to_iter
        |> map (fun l ->
               Assignment.Map.find_opt l a
               |> Option.map_or ~default:0 Assignment.level)
        |> max |> Option.get_or ~default:0
      in
      let i =
        Assignment.Implication { literal = l; implicant = ls'; level = d' }
      in
      let a' = Assignment.Map.add l i a in
      let t' = (i, f) :: t in
      let f' = { f with assignments = a'; trail = t' } in
      simplify f' l |> Result.flat_map unit_propagate
  | None -> Ok f

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
      match OccurrenceMap.find_opt (Literal.neg l) occur with
      | Some cs ->
          IntSet.fold
            (fun c (uc', f') ->
              let ls = ClauseMap.find c clauses in
              (* Printf.printf "Examining clause: %s\n" (Clause.show ls); *)
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
                        (* Printf.printf "Unassigned literal: %s\n" *)
                        (*   (Literal.show l); *)
                        (uc'', f'', Clause.add l unassigned', false, satisfied'))
                  ls
                  (uc', f', Clause.empty, true, false)
              in
              if satisfied then (uc'', f'')
              else if falsified then (
                Printf.printf "Falsified clause: %s\n" (Clause.show ls);
                raise_notrace (Conflict (ls, f)))
              else if Clause.size unassigned = 1 then (
                if
                  not
                    (List.mem
                       ~eq:(fun (_, c1) (_, c2) -> Clause.equal c1 c2)
                       (c, ls) uc')
                then
                  Printf.printf "Adding unit clause to queue: %s\n"
                    (Clause.show ls);
                ( List.add_nodup
                    ~eq:(fun (_, c1) (_, c2) -> Clause.equal c1 c2)
                    (c, ls) uc'',
                  f'' ))
              else (uc'', f''))
            cs (uc, f)
      | None -> (uc, f)
    in
    let frequency' =
      FrequencyMap.remove_literal l frequency
      |> FrequencyMap.remove_literal (Literal.neg l)
    in
    let f' = { f' with frequency = frequency'; unit_clauses = uc' } in
    Ok f'
  with Conflict (c, f) -> Error (c, f)

let rec unit_propagate ({ unit_clauses = ucs; assignments = a; _ } as f) =
  match ucs with
  | (_, uc) :: ucs' -> (
      (* print_endline (show f); *)
      (* Printf.printf "Attempting to unit propagate %s\n" (Clause.show uc); *)
      (* print_string "Rest of queue: "; *)
      (* List.iter (fun (_, c) -> print_string (Clause.show c ^ " ")) ucs'; *)
      (* print_newline (); *)
      (* Clause.to_iter uc *)
      (* |> Iter.iter (fun l -> *)
      (*        Printf.printf "%s: %b\n" (Literal.show l) (Assignment.Map.mem l a)); *)
      let f' = { f with unit_clauses = ucs' } in
      match
        Clause.to_iter uc
        |> Iter.find_pred (fun l -> not (Assignment.Map.mem l a))
      with
      | Some l ->
          Printf.printf "Unit propagating by literal %s taken from %s\n"
            (Literal.show l) (Clause.show uc);
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
  (* print_endline (show f); *)
  let l = choose_literal f in
  let dec = Assignment.Decision { literal = l; level = d + 1 } in
  let f' = make_assignment l dec f |> Result.get_exn in
  (* print_endline (show f'); *)
  { f' with current_decision_level = d + 1 }
(* let l = choose_literal f in *)
(* rewrite f l *)
