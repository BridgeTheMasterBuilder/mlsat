open Common

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
  (* include Literal.Set *)

  (* let of_list ls = Iter.(of_list ls |> map Literal.of_int |> of_iter) *)
  (* let size = cardinal *)

  (* let show c = *)
  (*   "(" ^ fold (fun l s -> Printf.sprintf "%s%s " s (Literal.show l)) c "" ^ ")" *)

  (* let to_set = Fun.id *)
  include List

  type t = Literal.t list

  let add = add_nodup ~eq:Literal.equal
  let choose = hd
  let fold f c s = fold_left (Fun.flip f) s c
  let of_list = sort_uniq ~cmp:Literal.compare
  let remove l = remove ~eq:Literal.equal ~key:l
  let size = length

  let show c =
    "("
    ^ fold_left (fun s l -> Printf.sprintf "%s%s " s (Literal.show l)) "" c
    ^ ")"

  let to_list = Fun.id
  let to_set = Literal.Set.of_list
end

module ClauseMap = struct
  include IntMap

  type t = Clause.t IntMap.t
  type key = Literal.t

  let show map_str =
    fold
      (fun c ls s -> Printf.sprintf "%s%d:%s\n" s c (Clause.show ls))
      map_str ""

  let size m = max_binding_opt m |> Option.map_or ~default:0 fst
end

module OccurrenceMap = struct
  include Literal.Map

  type t = IntSet.t Literal.Map.t
  type key = Literal.t

  let show map_str =
    fold
      (fun l cs s ->
        Printf.sprintf "%s%s:%s\n" s (Literal.show l)
          (IntSet.fold (fun l acc -> Printf.sprintf "%s%d " acc l) cs ""))
      map_str ""
end

module Assignment = struct
  type t =
    | Decision of { literal : Literal.t; level : int }
    | Implication of { literal : Literal.t; level : int; implicant : Clause.t }

  type assignment = t

  module Map = struct
    include Literal.Map

    type t = assignment Literal.Map.t

    let add l = add (Literal.var l)
    let find_opt l = get (Literal.var l)
  end

  let level = function
    | Decision { level; _ } | Implication { level; _ } -> level

  let literal = function
    | Decision { literal; _ } | Implication { literal; _ } -> literal

  let show (a : assignment) =
    match a with
    | Decision { literal; level } ->
        Printf.sprintf "\t%s because of decision on level %d\n"
          (Literal.show literal) level
    | Implication { literal; level; implicant } ->
        Printf.sprintf "\t%s because of clause (%s) - implied at level %d\n"
          (Literal.show literal) (Clause.show implicant) level

  let was_decided_on_level a d =
    match a with Decision { level = d'; _ } -> d = d' | _ -> false
end

type formula = {
  clauses : ClauseMap.t;
  occur : OccurrenceMap.t;
  original_clauses : ClauseMap.t;
  unit_clauses : ClauseMap.t;
  two_literal_clauses : ClauseMap.t;
  current_decision_level : int;
  assignments : Assignment.Map.t;
  trail : (Assignment.t * formula) list;
  database : Clause.t list;
}

let show_formula ({ clauses; occur; current_decision_level; database; _ } as f)
    =
  let open Printf in
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
    (if ClauseMap.is_empty clauses then "()" else ClauseMap.show clauses)
    (if OccurrenceMap.is_empty occur then "()" else OccurrenceMap.show occur)
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
              OccurrenceMap.update l
                (function
                  | Some s -> Some (IntSet.add n s)
                  | None -> Some (IntSet.singleton n))
                m)
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

let choose_literal { clauses; occur; two_literal_clauses = tlc; _ } =
  let m = if ClauseMap.is_empty tlc then clauses else tlc in
  let l = ClauseMap.choose m |> snd |> Clause.choose in
  l
(* match Literal.Map.find_opt (Literal.var l) a with *)
(* | Some _ -> choose_literal { f with occur = OccurrenceMap.remove l occur } *)
(* | None -> l *)
(* let m = if OccurrenceMap.is_empty tlc then clauses else tlc in  *)
(* let v = *)
(*   let open Iter in *)
(*   (\* (OccurrenceMap.filter_map (fun l cs -> if Literal.is_negated l then None else Some cs) occur) |> *\) *)
(*   OccurrenceMap.to_iter m *)
(*   |> filter_map (fun (l, cs) -> *)
(*          if Literal.is_negated l then None else Some (l, cs)) *)
(*   |> max ~lt:(fun (_, cs) (_, cs') -> *)
(*       IntSet.cardinal cs < IntSet.cardinal cs') *)
(* |> Option.map fst |> Option.get_exn_or "Impossible" *)
(*   (\* fst *\) *)
(*   (\*   (IntMap.max_binding *\) *)
(*   (\*   @@ IntMap.map *\) *)
(*   (\*        (fun v1 -> IntSet.cardinal v1) *\) *)
(*   (\*        (IntMap.mapKeys (fun k -> abs k) occur)) *\) *)
(*   (\*        ) *\) *)
(* in *)
(* let sz1 = *)
(*   Option.value *)
(*     (IntMap.find_opt v occur |> Option.map IntSet.cardinal) *)
(*     ~default:0 *)
(* in *)
(* let sz2 = *)
(*   Option.value *)
(*     (IntMap.find_opt (-v) occur |> Option.map IntSet.cardinal) *)
(*     ~default:0 *)
(* in *)
(* Literal (if sz1 > sz2 then v else -v) *)

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
                match ClauseMap.find_opt c clauses with
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
  let rec recur q c history =
    match CCFQueue.take_front q with
    | None -> c
    | Some (l, q') -> (
        (* if CCFQueue.is_empty q' then Clause.add l c *)
        (* else *)
        match Assignment.(Map.find_opt l a) with
        | Some (Decision _) -> recur q' (Clause.add l c) history
        | Some (Implication { implicant = ls'; level = d'; _ }) ->
            if d' < d then recur q' (Clause.add l c) history
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
        OccurrenceMap.update l
          (function
            | Some s -> Some (IntSet.add (n + 1) s)
            | None -> Some (IntSet.singleton (n + 1)))
          m)
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
  (* let d' = *)
  (*   let open Iter in *)
  (*   Clause.to_iter learned_clause *)
  (*   |> filter_map (fun l -> *)
  (*          Assignment.Map.find_opt l a *)
  (*          |> Option.flat_map (fun ass -> *)
  (*                 let d' = Assignment.level ass in *)
  (*                 d' < d |> Bool.if_then (Fun.const d'))) *)
  (*   |> max *)
  (*   |> Option.value ~default:0 *)
  (* in *)
  let _, f' =
    if d' = 0 then List.last_opt t |> Option.get_exn_or "TRAIL"
    else List.find (fun (ass, _) -> Assignment.was_decided_on_level ass d') t
  in
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

(* let my_assert assertions = *)
(*   let rec recur assertions has_error = *)
(*     match assertions with *)
(*     | (c, msg) :: t -> *)
(*         if not c then Printf.printf "ASSERTION FAILED: %s\n" msg; *)
(*         recur t (has_error || not c) *)
(*     | [] -> assert (not has_error) *)
(*   in *)
(*   recur assertions false *)

(* let check_invariants *)
(*     ({ *)
(*        clauses = cm; *)
(*        occur = lm; *)
(*        original_clauses = oc; *)
(*        current_decision_level = d; *)
(*        assignments = a; *)
(*        trail = t; *)
(*        database = db; *)
(*        _; *)
(*      } as f) = *)
(*   let open Iter in *)
(*   let no_empty_clauses = *)
(*     not (ClauseMap.to_iter cm |> exists (fun (_, c) -> Clause.is_empty c)) *)
(*   in *)
(*   let is_subset_of_original = *)
(*     ClauseMap.to_iter cm *)
(*     |> for_all (fun (k, v) -> *)
(*            match ClauseMap.find_opt k oc with *)
(*            | Some v' -> Clause.subset v v' *)
(*            | None -> false) *)
(*   in *)
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
(*     ClauseMap.to_iter cm *)
(*     |> for_all (fun (c, ls) -> *)
(*            Clause.to_iter ls *)
(*            |> for_all (fun l -> *)
(*                   IntSet.to_iter (OccurrenceMap.find l lm) *)
(*                   |> for_all (fun c' -> Clause.mem l (ClauseMap.find c' cm)) *)
(*                   && IntSet.mem c (OccurrenceMap.find l lm))) *)
(*   in *)
(*   let literals_clauses_eq = *)
(*     OccurrenceMap.to_iter lm *)
(*     |> for_all (fun (l, cs) -> *)
(*            IntSet.to_iter cs *)
(*            |> for_all (fun c -> *)
(*                   Clause.to_iter (ClauseMap.find c cm) *)
(*                   |> for_all (fun l' -> IntSet.mem c (OccurrenceMap.find l' lm)) *)
(*                   && Clause.mem l (ClauseMap.find c cm))) *)
(*   in *)
(*   let learned_clauses_no_empty_clauses = *)
(*     not (List.to_iter db |> exists (fun c -> Clause.is_empty c)) *)
(*   in *)
(*   my_assert *)
(*     [ *)
(*       (no_empty_clauses, "Formula contains empty clause"); *)
(*       (is_subset_of_original, "Formula has diverged from its original form"); *)
(*       (decision_level_non_negative, "Decision level is not non-negative"); *)
(*       (assignments_valid, "Assignments are invalid"); *)
(*       (trail_valid, "Trail has duplicate assignments:\n" ^ show_formula f); *)
(*       ( trail_geq_decision_level, *)
(*         "Fewer assignments than decision levels in trail" ); *)
(*       (clauses_literals_eq, "Clauses and literals out of sync"); *)
(*       (literals_clauses_eq, "Literals and clauses out of sync"); *)
(*       (learned_clauses_no_empty_clauses, "Learned empty clause"); *)
(*     ]; *)
(*   () *)
