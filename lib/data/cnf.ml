open Common

(* module Literal = struct *)
(*   type t = int *)

(*   let neg = ( ~- ) *)
(*   let show l = string_of_int l *)
(*   let var = abs *)
(* end *)
module Literal = struct
  module Map = Map.Make (Int)
  module Set = Iter.Set.Make (Int)
  include Int

  let invalid = 0
  let is_negated l = l < 0
  let of_int i = if i = 0 then failwith "Invalid literal" else i
  let show = string_of_int
  let signum = sign
  let var = abs
end

module Clause = struct
  include Literal.Set

  let of_list ls = Iter.(of_list ls |> map Literal.of_int |> of_iter)
  let size = cardinal

  let show c =
    "("
    ^ fold
        (fun l s ->
          (if String.equal s "" then "" else s ^ "\\/") ^ Literal.show l)
        c ""
    ^ ")"

  let to_set = Fun.id
end

type assignment =
  | Decision of { literal : Literal.t; level : int }
  | Implication of { literal : Literal.t; level : int; implicant : Clause.t }

module ClauseMap = struct
  include IntMap

  type t = Clause.t IntMap.t
  type key = Literal.t

  let size m = max_binding_opt m |> Option.map_or ~default:0 fst
end

module OccurrenceMap = struct
  include Literal.Map

  type t = IntSet.t Literal.Map.t
  type key = Literal.t
end

type formula = {
  clauses : ClauseMap.t;
  (* occur : IntSet.t IntMap.t; *)
  occur : OccurrenceMap.t;
  original_clauses : ClauseMap.t;
  unit_clauses : ClauseMap.t;
  two_literal_clauses : ClauseMap.t;
  current_decision_level : int;
  (* assignments : assignment IntMap.t; *)
  assignments : assignment Literal.Map.t;
  trail : (assignment * formula) list;
  database : Clause.t list;
}

let show_assignment (a : assignment) =
  let open Printf in
  match a with
  | Decision { literal; level } ->
      sprintf "\t%s because of decision on level %d\n" (Literal.show literal)
        level
  | Implication { literal; level; implicant } ->
      let implicant_str =
        (* IntSet.fold (fun l acc -> sprintf "%s%d " acc l) implicant "" *)
        Clause.show implicant
      in
      sprintf "\t%s because of clause (%s) - implied at level %d\n"
        (Literal.show literal) implicant_str level

let show_formula ({ clauses; occur; current_decision_level; database; _ } as f)
    =
  let open Printf in
  let show_clauses map_str =
    ClauseMap.to_list map_str
    |> List.map (fun (k, v) ->
           let clause_str =
             sprintf "(%s)"
               (Clause.fold
                  (fun l acc -> sprintf "%s%s " acc (Literal.show l))
                  v "")
           in
           sprintf "%d:%s\n" k clause_str)
    |> String.concat ""
  in
  let show_occur map_str =
    OccurrenceMap.to_list map_str
    |> List.map (fun (k, v) ->
           let clause_str =
             sprintf "(%s)"
               (IntSet.fold (fun l acc -> sprintf "%s%d " acc l) v "")
           in
           sprintf "%s:%s\n" (Literal.show k) clause_str)
    |> String.concat ""
  in

  sprintf
    "Clauses:\n\
     %s\n\
     Literals:\n\
     %s\n\
     Decision level: %d\n\
     Assignments:\n\
     %s\n\
     Learned clauses:\n\
     %s"
    (if ClauseMap.is_empty clauses then "()" else show_clauses clauses)
    (if OccurrenceMap.is_empty occur then "()" else show_occur occur)
    current_decision_level
    (List.fold_left
       (fun acc (ass, _) -> sprintf "%s%s" acc (show_assignment ass))
       "" f.trail)
    (List.fold_left
       (fun acc c ->
         sprintf "%s(%s)\n" acc
           (Clause.fold
              (fun l acc -> sprintf "%s%s " acc (Literal.show l))
              c ""))
       "" database)

let of_list =
  let rec recur
      ({ clauses; occur; unit_clauses = uc; two_literal_clauses = tlc; _ } as f)
      n = function
    | [] -> f
    | c :: cs ->
        let clause = Clause.of_list c in
        let clauses' = ClauseMap.add n clause clauses in
        let occur' =
          Clause.fold
            (fun l m ->
              OccurrenceMap.add_list_with
                ~f:(fun _ -> IntSet.union)
                m
                [ (l, IntSet.singleton n) ])
            clause occur
        in
        let uc', tlc' =
          match Clause.size clause with
          | 1 -> (ClauseMap.add n clause uc, tlc)
          | 2 -> (uc, ClauseMap.add n clause tlc)
          | _ -> (uc, tlc)
        in
        let f' =
          {
            f with
            clauses = clauses';
            occur = occur';
            original_clauses = clauses';
            two_literal_clauses = tlc';
            unit_clauses = uc';
          }
        in

        recur f' (n + 1) cs
  in
  recur
    {
      clauses = ClauseMap.empty;
      occur = OccurrenceMap.empty;
      original_clauses = ClauseMap.empty;
      current_decision_level = 0;
      assignments = Literal.Map.empty;
      trail = [];
      database = [];
      unit_clauses = ClauseMap.empty;
      two_literal_clauses = ClauseMap.empty;
    }
    1

let is_empty { clauses; _ } = ClauseMap.is_empty clauses

let rec choose_literal
    ({ occur; two_literal_clauses = _tlc; assignments = a; _ } as f) =
  let l = OccurrenceMap.choose occur |> fst in
  match Literal.Map.find_opt (Literal.var l) a with
  | Some _ -> choose_literal { f with occur = OccurrenceMap.remove l occur }
  | None -> l

(* let v = *)
(*   fst *)
(*     (IntMap.max_binding *)
(*     @@ IntMap.map *)
(*          (fun v1 -> IntSet.cardinal v1) *)
(*          (IntMap.mapKeys (fun k -> abs k) lm)) *)
(* in *)
(* let sz1 = *)
(*   Option.value (IntMap.find_opt v lm |> Option.map IntSet.cardinal) ~default:0 *)
(* in *)
(* let sz2 = *)
(*   Option.value *)
(*     (IntMap.find_opt (-v) lm |> Option.map IntSet.cardinal) *)
(*     ~default:0 *)
(* in *)
(* Literal (if sz1 > sz2 then v else -v) *)
let level = function Decision { level; _ } | Implication { level; _ } -> level

let raw_delete_literal ({ occur; _ } as f) l =
  { f with occur = OccurrenceMap.remove l occur }

let delete_literal ({ occur; original_clauses = oc; _ } as f) l =
  match OccurrenceMap.find_opt l occur with
  | None -> Ok f
  | Some cs ->
      let result =
        IntSet.fold
          (fun c f' ->
            match f' with
            | Error x -> Error x
            | Ok
                ({ clauses; unit_clauses = uc; two_literal_clauses = tlc; _ } as
                f'') -> (
                match ClauseMap.get c clauses with
                | None -> Ok f''
                | Some ls -> (
                    let diff = Clause.remove l ls in
                    match Clause.size diff with
                    | 0 -> Error (ClauseMap.find c oc, f)
                    | 1 ->
                        Ok
                          {
                            f'' with
                            clauses = ClauseMap.add c diff clauses;
                            unit_clauses = ClauseMap.add c diff uc;
                            two_literal_clauses = ClauseMap.remove c tlc;
                          }
                    | 2 ->
                        Ok
                          {
                            f'' with
                            clauses = ClauseMap.add c diff clauses;
                            two_literal_clauses = ClauseMap.add c diff tlc;
                          }
                    | _ ->
                        Ok { f'' with clauses = ClauseMap.add c diff clauses })))
          cs (Ok f)
      in
      let lm' = OccurrenceMap.remove l occur in
      Result.map (fun f' -> { f' with occur = lm' }) result

let delete_clauses f cs =
  IntSet.fold
    (fun c
         ({ clauses; occur; unit_clauses = uc; two_literal_clauses = tlc; _ } as
         f') ->
      let ls = ClauseMap.find c clauses in
      let occur' =
        Clause.fold
          (fun l m ->
            let diff = IntSet.remove c (OccurrenceMap.find l m) in
            if IntSet.is_empty diff then OccurrenceMap.remove l m
            else OccurrenceMap.add l diff m)
          ls occur
      in
      let clauses' = ClauseMap.remove c clauses in
      let tlc' = ClauseMap.remove c tlc in
      let uc' = ClauseMap.remove c uc in
      {
        f' with
        clauses = clauses';
        occur = occur';
        unit_clauses = uc';
        two_literal_clauses = tlc';
      })
    cs f

let simplify ({ occur; _ } as f) l =
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
               Literal.(Map.get (var l) a)
               |> Option.map_or ~default:0 (function
                      | Decision { level; _ } | Implication { level; _ } ->
                      level))
        |> max |> Option.get_or ~default:0
      in
      let i = Implication { literal = l'; implicant = ls'; level = d' } in
      let a' = Literal.Map.add (Literal.var l') i a in
      let t' = (i, f) :: t in
      let f' = { f with assignments = a'; trail = t' } in
      simplify f' l' |> Result.flat_map unit_propagate
  | None -> Ok f

let rewrite ({ current_decision_level = d; assignments = a; trail = t; _ } as f)
    l =
  let f' = Result.get_exn (simplify f l) in
  let a' =
    Literal.Map.add (Literal.var l) (Decision { literal = l; level = d + 1 }) a
  in
  let t' = (Decision { literal = l; level = d + 1 }, f) :: t in
  { f' with current_decision_level = d + 1; assignments = a'; trail = t' }

let analyze_conflict { current_decision_level = d; assignments = a; _ } clause =
  let ls = Literal.Set.elements clause in
  let rec recur q c history =
    match CCFQueue.take_front q with
    | None -> c
    | Some (l, q') -> (
        match IntMap.find_opt (Literal.var l) a with
        | Some (Decision _) -> recur q' (Clause.add l c) history
        | Some (Implication { implicant = ls'; level = d'; _ }) ->
            if d' < d then recur q' (Clause.add l c) history
            else
              let ls'' =
                Clause.filter
                  (fun l'' -> not (Literal.Set.mem (Literal.var l'') history))
                  ls'
              in
              let q'' = CCFQueue.add_iter_back q' (Clause.to_iter ls'') in
              let history' =
                Literal.Set.union history
                  (Literal.Set.map Literal.var (Clause.to_set ls''))
              in
              recur q'' c history'
        | _ -> recur q' c history)
  in
  recur (CCFQueue.of_list ls) Clause.empty
    (Literal.Set.of_list (List.map Literal.var ls))

let maximumMay x = List.to_iter x |> Iter.max

let add_clause
    ({
       clauses;
       occur;
       original_clauses = oc;
       unit_clauses = uc;
       two_literal_clauses = tlc;
       _;
     } as f) clause original_clause =
  let n = ClauseMap.size oc in
  let clauses' = ClauseMap.add (n + 1) clause clauses in
  let occur' =
    Clause.fold
      (fun l m ->
        OccurrenceMap.add_list_with
          ~f:(fun _ -> IntSet.union)
          m
          [ (l, IntSet.singleton (n + 1)) ])
      clause occur
  in
  let oc' = ClauseMap.add (n + 1) original_clause oc in
  let uc', tlc' =
    match Clause.size clause with
    | 1 -> (ClauseMap.add (n + 1) clause uc, tlc)
    | 2 -> (uc, ClauseMap.add (n + 1) clause tlc)
    | _ -> (uc, tlc)
  in
  {
    f with
    clauses = clauses';
    occur = occur';
    original_clauses = oc';
    unit_clauses = uc';
    two_literal_clauses = tlc';
  }

let literal = function
  | Decision { literal; _ } | Implication { literal; _ } -> literal

let add_learned_clauses ({ assignments = a; _ } as f) db =
  let db' =
    List.map
      (fun c ->
        Clause.fold
          (fun l c' ->
            match Literal.Map.find_opt (Literal.var l) a with
            | Some x ->
                if Literal.signum (literal x) <> Literal.signum l then
                  Option.map (fun c' -> Clause.remove l c') c'
                else None
            | _ -> c')
          c (Some c))
      db
  in
  let db'', oc' =
    List.split
      (List.map
         (fun (c1, c2) -> (Option.get_exn_or "SPLIT" c1, c2))
         (List.filter (fun (c1, _) -> Option.is_some c1) (List.combine db' db)))
  in
  let add_clause' f'' (c, oc) = add_clause f'' c oc in
  let f' = List.fold_left add_clause' f (List.combine db'' oc') in
  { f' with database = db }

let backtrack
    { current_decision_level = d; assignments = a; trail = t; database = db; _ }
    learned_clause =
  let ds =
    List.map level
      (List.filter_map
         (fun l -> Literal.Map.find_opt (Literal.var l) a)
         (Clause.to_list learned_clause))
  in
  let ds' = List.filter (fun d' -> d' < d) ds in
  let d' = Option.value (maximumMay ds') ~default:0 in
  let _, f' =
    if d' = 0 then List.last_opt t |> Option.get_exn_or "TRAIL"
    else
      List.find
        (fun (ass, _) ->
          match ass with Decision { level = d''; _ } -> d'' = d' | _ -> false)
        t
  in
  let f'' = add_learned_clauses f' (learned_clause :: db) in
  (f'', d')

let restart ({ trail = t; database = db; _ } as f) =
  if List.is_empty t then f
  else
    add_learned_clauses
      (snd (List.last_opt t |> Option.get_exn_or "RESTART"))
      db

let show_set c =
  "("
  ^ Clause.fold
      (fun l s ->
        (if String.equal s "" then "" else s ^ "\\/") ^ Literal.show l)
      c ""
  ^ ")"

let show_trail trail =
  List.fold_left
    (fun s x ->
      s ^ " "
      ^
      match x with
      | Decision { literal = l; level = lv } ->
          "\t" ^ Literal.show l ^ " because of decision on level "
          ^ string_of_int lv ^ "\n"
      | Implication { literal = l; level = lv; implicant = i } ->
          "\t" ^ Literal.show l ^ " because of clause " ^ show_set i
          ^ " - implied at level " ^ string_of_int lv ^ "\n")
    "" (List.map fst trail)

let myAssert assertions =
  let open Printf in
  let rec recur assertions has_error =
    match assertions with
    | (c, msg) :: t ->
        if not c then printf "ASSERTION FAILED: %s\n" msg;
        recur t (has_error || not c)
    | [] -> assert (not has_error)
  in
  recur assertions false

let check_invariants
    ({
       clauses = cm;
       occur = lm;
       current_decision_level = d;
       assignments = a;
       trail = t;
       database = db;
       _;
     } as f) =
  let open List in
  let no_empty_clauses =
    not (exists (fun (_, c) -> Clause.is_empty c) (ClauseMap.to_list cm))
  in
  (* let is_subset_of_original = is_submap (fun _ _ -> true) cm oc in *)
  let decision_level_non_negative = d >= 0 in
  let assignments_valid =
    for_all
      (fun (k, v) -> Literal.equal k (Literal.var (literal v)))
      (Literal.Map.bindings a)
  in
  let trail_valid =
    for_all
      (fun (x, _) ->
        let x' = literal x in
        not
          (exists
             (fun (y, _) ->
               let y' = literal y in
               Literal.equal x' (Literal.neg y'))
             t))
      t
  in
  let trail_geq_decision_level = length t >= d in
  let clauses_literals_eq =
    for_all
      (fun (c, ls) ->
        for_all
          (fun l ->
            for_all
              (fun c' -> Clause.mem l (ClauseMap.find c' cm))
              (IntSet.to_list (OccurrenceMap.find l lm))
            && IntSet.mem c (OccurrenceMap.find l lm))
          (Clause.to_list ls))
      (ClauseMap.to_list cm)
  in
  let literals_clauses_eq =
    for_all
      (fun (l, cs) ->
        for_all
          (fun c ->
            for_all
              (fun l' -> IntSet.mem c (OccurrenceMap.find l' lm))
              (Clause.to_list (ClauseMap.find c cm))
            && Clause.mem l (ClauseMap.find c cm))
          (IntSet.to_list cs))
      (OccurrenceMap.to_list lm)
  in
  let learned_clauses_no_empty_clauses =
    not (exists (fun c -> Clause.is_empty c) db)
  in
  myAssert
    [
      (no_empty_clauses, "Formula contains empty clause");
      (* (is_subset_of_original, "Formula has diverged from its original form"); *)
      (decision_level_non_negative, "Decision level is not non-negative");
      (assignments_valid, "Assignments are invalid");
      (trail_valid, "Trail has duplicate assignments:\n" ^ show_formula f);
      ( trail_geq_decision_level,
        "Fewer assignments than decision levels in trail" );
      (clauses_literals_eq, "Clauses and literals out of sync");
      (literals_clauses_eq, "Literals and clauses out of sync");
      (learned_clauses_no_empty_clauses, "Learned empty clause");
    ];
  ()
