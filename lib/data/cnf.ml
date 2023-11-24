open Common

module Literal = struct
  type t = int

  let show l = string_of_int l
  let var = abs
end

type assignment =
  | Decision of { literal : int; level : int }
  | Implication of { literal : int; level : int; implicant : IntSet.t }

type formula = {
  clauseLiterals : IntSet.t IntMap.t;
  literalClauses : IntSet.t IntMap.t;
  originalClauses : IntSet.t IntMap.t;
  currentDecisionLevel : int;
  assignments : assignment IntMap.t;
  trail : (assignment * formula) list;
  learnedClauses : IntSet.t list;
  twoLiteralClauses : IntSet.t IntMap.t;
  unitClauses : IntSet.t IntMap.t;
}

let show_assignment (a : assignment) =
  let open Printf in
  match a with
  | Decision { literal; level } ->
      sprintf "\t%d because of decision on level %d\n" literal level
  | Implication { literal; level; implicant } ->
      let implicant_str =
        IntSet.fold (fun l acc -> sprintf "%s%d " acc l) implicant ""
      in
      sprintf "\t%d because of clause (%s) - implied at level %d\n" literal
        implicant_str level

let show_formula (f : formula) =
  let open Printf in
  let show_clauses map_str =
    IntMap.bindings map_str
    |> List.map (fun (k, v) ->
           let clause_str =
             sprintf "(%s)"
               (IntSet.fold (fun l acc -> sprintf "%s%d " acc l) v "")
           in
           sprintf "%d:%s\n" k clause_str)
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
    (if IntMap.is_empty f.clauseLiterals then "()"
     else show_clauses f.clauseLiterals)
    (if IntMap.is_empty f.literalClauses then "()"
     else show_clauses f.literalClauses)
    f.currentDecisionLevel
    (List.fold_left
       (fun acc (ass, _) -> sprintf "%s%s" acc (show_assignment ass))
       "" f.trail)
    (List.fold_left
       (fun acc c ->
         sprintf "%s(%s)\n" acc
           (IntSet.fold (fun l acc -> sprintf "%s%d " acc l) c ""))
       "" f.learnedClauses)

let myAssert assertions =
  let open Printf in
  let rec recur assertions hasError =
    match assertions with
    | (c, msg) :: t ->
        if not c then printf "ASSERTION FAILED: %s\n" msg;
        recur t (hasError || not c)
    | [] -> assert (not hasError)
  in
  recur assertions false

let of_list clauses =
  let rec recur f n clauses =
    match clauses with
    | [] -> f
    | clause :: rest ->
        let lits = IntSet.of_list clause in
        let cm' = IntMap.add n lits f.clauseLiterals in
        let lm' =
          IntSet.fold
            (fun l m ->
              IntMap.add_list_with
                ~f:(fun _ -> IntSet.union)
                m
                [ (l, IntSet.singleton n) ])
            lits f.literalClauses
        in
        let uc', tlc' =
          match List.length clause with
          | 1 -> (IntMap.add n lits f.unitClauses, f.twoLiteralClauses)
          | 2 -> (f.unitClauses, IntMap.add n lits f.twoLiteralClauses)
          | _ -> (f.unitClauses, f.twoLiteralClauses)
        in
        let f' =
          {
            f with
            clauseLiterals = cm';
            literalClauses = lm';
            originalClauses = cm';
            twoLiteralClauses = tlc';
            unitClauses = uc';
          }
        in

        recur f' (n + 1) rest
  in
  recur
    {
      clauseLiterals = IntMap.empty;
      literalClauses = IntMap.empty;
      originalClauses = IntMap.empty;
      currentDecisionLevel = 0;
      assignments = IntMap.empty;
      trail = [];
      learnedClauses = [];
      twoLiteralClauses = IntMap.empty;
      unitClauses = IntMap.empty;
    }
    1 clauses

let null { clauseLiterals = cm; literalClauses = lm; _ } =
  IntMap.is_empty cm || IntMap.is_empty lm

let rec choose_literal
    ({ literalClauses = lm; twoLiteralClauses = tlc; assignments = a; _ } as f)
    =
  print_endline ("Making a decision:\n" ^ show_formula f);
  let l = IntMap.choose lm |> fst in
  match IntMap.find_opt (Literal.var l) a with
  | Some _ -> choose_literal { f with literalClauses = IntMap.remove l lm }
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

let raw_delete_literal f l =
  { f with literalClauses = IntMap.remove l f.literalClauses }

let delete_literal f l =
  match IntMap.find_opt l f.literalClauses with
  | None -> Ok f
  | Some clauses ->
      let result =
        IntSet.fold
          (fun c f' ->
            match f' with
            | Error x -> Error x
            | Ok f'' -> (
                match IntMap.find_opt c f''.clauseLiterals with
                | None -> Ok f''
                | Some ls -> (
                    let diff = IntSet.remove l ls in
                    match IntSet.cardinal diff with
                    | 0 -> Error (IntMap.find c f.originalClauses, f)
                    | 1 ->
                        Ok
                          {
                            f'' with
                            clauseLiterals =
                              IntMap.add c diff f''.clauseLiterals;
                            unitClauses = IntMap.add c diff f''.unitClauses;
                            twoLiteralClauses =
                              IntMap.remove c f''.twoLiteralClauses;
                          }
                    | 2 ->
                        Ok
                          {
                            f'' with
                            clauseLiterals =
                              IntMap.add c diff f''.clauseLiterals;
                            twoLiteralClauses =
                              IntMap.add c diff f''.twoLiteralClauses;
                          }
                    | _ ->
                        Ok
                          {
                            f'' with
                            clauseLiterals =
                              IntMap.add c diff f''.clauseLiterals;
                          })))
          clauses (Ok f)
      in
      let lm' = IntMap.remove l f.literalClauses in
      Result.map (fun f' -> { f' with literalClauses = lm' }) result

let delete_clauses f clauses =
  IntSet.fold
    (fun c f' ->
      let lits = IntMap.find c f'.clauseLiterals in
      let lm' =
        IntSet.fold
          (fun l m ->
            let diff = IntSet.remove c (IntMap.find l m) in
            if IntSet.is_empty diff then IntMap.remove l m
            else IntMap.add l diff m)
          lits f'.literalClauses
      in
      let cm' = IntMap.remove c f'.clauseLiterals in
      let tlc' = IntMap.remove c f'.twoLiteralClauses in
      let uc' = IntMap.remove c f'.unitClauses in
      {
        f' with
        clauseLiterals = cm';
        literalClauses = lm';
        twoLiteralClauses = tlc';
        unitClauses = uc';
      })
    clauses f

(* let simplify ({ literalClauses = lm; _ } as f) l = *)
(*   match delete_literal f (-l) with Error x -> Error x | Ok f'' -> Ok f'' *)
let simplify f l =
  delete_literal f (-l)
  |> Result.map (fun f' ->
         match IntMap.find_opt l f.literalClauses with
         | None -> f'
         | Some clauses -> raw_delete_literal (delete_clauses f' clauses) l)

let rec unit_propagate
    ({ assignments = a; originalClauses = oc; trail = t; unitClauses = uc; _ }
    as f) =
  match IntMap.min_binding_opt uc with
  | Some (c, ls) ->
      let l' = IntSet.min_elt ls in
      let ls' = IntMap.find c oc in
      let d' =
        Option.value
          (IntSet.max_elt_opt
             (IntSet.map
                (fun l ->
                  Option.value
                    (IntMap.find_opt (Literal.var l) a |> Option.map level)
                    ~default:0)
                (IntSet.remove l' ls')))
          ~default:0
      in

      let i = Implication { literal = l'; implicant = ls'; level = d' } in
      let a' = IntMap.add (Literal.var l') i a in
      let t' = (i, f) :: t in
      let f' = { f with assignments = a'; trail = t' } in
      (* match simplify f' l' with *)
      (* | Left f'' -> unit_propagate f'' *)
      (* | Right _ -> Right f) *)
      simplify f' l' |> Result.flat_map unit_propagate
  | None -> Ok f

let rewrite ({ currentDecisionLevel = d; assignments = a; trail = t; _ } as f) l
    =
  let f' = Result.get_exn (simplify f l) in
  let a' =
    IntMap.add (Literal.var l) (Decision { literal = l; level = d + 1 }) a
  in
  let t' = (Decision { literal = l; level = d + 1 }, f) :: t in
  { f' with currentDecisionLevel = d + 1; assignments = a'; trail = t' }

let neg i = -i
let var i = abs i

let analyze_conflict f clause =
  let ls = IntSet.elements clause in
  let rec recur q c history =
    match CCFQueue.take_front q with
    | None -> c
    | Some (l, q') -> (
        match IntMap.find_opt (Literal.var l) f.assignments with
        | Some (Decision _) -> recur q' (IntSet.add l c) history
        | Some (Implication { implicant = ls'; level = d'; _ }) ->
            if d' < f.currentDecisionLevel then
              recur q' (IntSet.add l c) history
            else
              let ls'' =
                IntSet.filter
                  (fun l'' -> not (IntSet.mem (Literal.var l'') history))
                  ls'
              in
              let q'' = CCFQueue.add_iter_back q' (IntSet.to_iter ls'') in
              let history' =
                IntSet.union history (IntSet.map Literal.var ls'')
              in
              recur q'' c history'
        | _ -> recur q' c history)
  in
  recur (CCFQueue.of_list ls) IntSet.empty
    (IntSet.of_list (List.map Literal.var ls))

let maximumMay x = List.to_iter x |> Iter.max

let add_clause f clause original_clause =
  let n = fst (IntMap.max_binding f.originalClauses) in
  let cm' = IntMap.add (n + 1) clause f.clauseLiterals in
  let lm' =
    IntSet.fold
      (fun l m ->
        IntMap.add_list_with
          ~f:(fun _ -> IntSet.union)
          m
          [ (l, IntSet.singleton (n + 1)) ])
      clause f.literalClauses
  in
  let oc' = IntMap.add (n + 1) original_clause f.originalClauses in
  let uc', tlc' =
    match IntSet.cardinal clause with
    | 1 -> (IntMap.add (n + 1) clause f.unitClauses, f.twoLiteralClauses)
    | 2 -> (f.unitClauses, IntMap.add (n + 1) clause f.twoLiteralClauses)
    | _ -> (f.unitClauses, f.twoLiteralClauses)
  in
  {
    f with
    clauseLiterals = cm';
    literalClauses = lm';
    originalClauses = oc';
    twoLiteralClauses = tlc';
    unitClauses = uc';
  }

let literal = function
  | Decision { literal; _ } | Implication { literal; _ } -> literal

let signum = Int.sign

let add_learned_clauses f lc =
  let lc' =
    List.map
      (fun c ->
        IntSet.fold
          (fun l c' ->
            match IntMap.find_opt (var l) f.assignments with
            | Some x ->
                if signum (literal x) <> signum l then
                  Option.map (fun c' -> IntSet.remove l c') c'
                else None
            | _ -> c')
          c (Some c))
      lc
  in
  let lc'', oc' =
    List.split
      (List.map
         (fun (c1, c2) -> (Option.get_exn_or "SPLIT" c1, c2))
         (List.filter (fun (c1, _) -> Option.is_some c1) (List.combine lc' lc)))
  in
  let add_clause' f'' (c, oc) = add_clause f'' c oc in
  let f' = List.fold_left add_clause' f (List.combine lc'' oc') in
  { f' with learnedClauses = lc }

let backtrack f learned_clause =
  let ds =
    List.map level
      (List.filter_map
         (fun l -> IntMap.find_opt (Literal.var l) f.assignments)
         (IntSet.elements learned_clause))
  in
  let ds' = List.filter (fun d -> d < f.currentDecisionLevel) ds in
  let d' = Option.value (maximumMay ds') ~default:0 in
  let _, f' =
    if d' = 0 then List.last_opt f.trail |> Option.get_exn_or "TRAIL"
    else
      List.find
        (fun (ass, _) ->
          match ass with Decision { level = d''; _ } -> d'' = d' | _ -> false)
        f.trail
  in
  let f'' = add_learned_clauses f' (learned_clause :: f.learnedClauses) in
  (f'', d')

let restart f =
  if List.is_empty f.trail then f
  else
    add_learned_clauses
      (snd (List.last_opt f.trail |> Option.get_exn_or "RESTART"))
      f.learnedClauses

let show_set c =
  "("
  ^ IntSet.fold
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

let check_invariants
    ({
       clauseLiterals = cm;
       literalClauses = lm;
       originalClauses = oc;
       currentDecisionLevel = d;
       assignments = a;
       trail = t;
       learnedClauses = lc;
       _;
     } as f) =
  let open List in
  let no_empty_clauses =
    not (exists (fun (_, c) -> IntSet.is_empty c) (IntMap.bindings cm))
  in
  (* let is_subset_of_original = is_submap (fun _ _ -> true) cm oc in *)
  let decision_level_non_negative = d >= 0 in
  let assignments_valid =
    for_all (fun (k, v) -> k = var (literal v)) (IntMap.bindings a)
  in
  let trail_valid =
    for_all
      (fun (x, _) ->
        let x' = literal x in
        not
          (exists
             (fun (y, _) ->
               let y' = literal y in
               x' = -y')
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
              (fun c' -> IntSet.mem l (IntMap.find c' cm))
              (IntSet.to_list (IntMap.find l lm))
            && IntSet.mem c (IntMap.find l lm))
          (IntSet.to_list ls))
      (IntMap.bindings cm)
  in
  let literals_clauses_eq =
    for_all
      (fun (l, cs) ->
        for_all
          (fun c ->
            for_all
              (fun l' -> IntSet.mem c (IntMap.find l' lm))
              (IntSet.to_list (IntMap.find c cm))
            && IntSet.mem l (IntMap.find c cm))
          (IntSet.to_list cs))
      (IntMap.to_list lm)
  in
  let learned_clauses_no_empty_clauses =
    not (exists (fun c -> IntSet.is_empty c) lc)
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
