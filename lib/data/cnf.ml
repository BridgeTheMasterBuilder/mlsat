open Common

type assignment =
  | Decision of { literal : Literal.t; level : int }
  | Implication of { literal : Literal.t; level : int; implicant : Clause.t }

module ClauseMap = struct
  include IntMap

  type t = Clause.t IntMap.t

  exception Empty_clause of Clause.t

  let size = cardinal

  let show m =
    fold
      (fun c v s ->
        (if String.equal s "" then "" else s ^ "/\\")
        ^ string_of_int c ^ ":" ^ Clause.show v)
      m ""
end

module OccurrenceMap = struct
  include Literal.Map

  type t = IntSet.t Literal.Map.t

  let show m =
    fold
      (fun l pos s -> s ^ Literal.show l ^ " -> (" ^ IntSet.show pos ^ ")\n")
      m ""
end

type formula = {
  clauses : ClauseMap.t;
  original_clauses : ClauseMap.t;
  unit_clauses : ClauseMap.t;
  two_literal_clauses : ClauseMap.t;
  occur : OccurrenceMap.t;
  decision_level : int;
  assignments : assignment Literal.Map.t;
  trail : (assignment * formula) list;
  database : Clause.t list;
}

let check_invariants
    {
      clauses;
      original_clauses;
      occur;
      decision_level;
      assignments;
      trail;
      database;
      _;
    } =
  try
    print_endline "Checking invariants";
    let clauses_occurrences_eq =
      ClauseMap.for_all
        (fun c ls ->
          Clause.for_all
            (fun l ->
              match OccurrenceMap.get l occur with
              | None ->
                  print_endline
                    ("Literal " ^ Literal.show l
                   ^ " does not occur in any clause.");
                  assert false
              | Some cs ->
                  IntSet.for_all
                    (fun c ->
                      match ClauseMap.get c clauses with
                      | None ->
                          print_endline ("no such clause: " ^ string_of_int c);
                          assert false
                      | Some clauses ->
                          if not (Clause.mem l clauses) then
                            print_endline
                              ("Literal " ^ Literal.show l ^ " not in clause "
                             ^ string_of_int c);
                          assert (Clause.mem l clauses);
                          true)
                    cs
                  &&
                  (if not (IntSet.mem c cs) then
                     print_endline
                       ("Clause " ^ string_of_int c ^ " not in occurrence of "
                      ^ Literal.show l);
                   assert (IntSet.mem c cs);
                   true))
            ls)
        clauses
    in
    let occurrences_clauses_eq =
      OccurrenceMap.for_all
        (fun l pos ->
          IntSet.for_all
            (fun c ->
              (* let ls = ClauseMap.find c clauses in *)
              match ClauseMap.get c clauses with
              | None -> assert false
              | Some ls ->
                  Clause.for_all
                    (fun l ->
                      match OccurrenceMap.get l occur with
                      | None -> assert false
                      | Some pos ->
                          assert (IntSet.mem c pos);
                          true)
                    ls
                  &&
                  (assert (Literal.is_negated l || Clause.mem l ls);
                   true))
            pos)
        occur
    in
    assert (clauses_occurrences_eq && occurrences_clauses_eq);
    print_endline "OK"
  with Assert_failure (s, l, c) ->
    print_endline
      (s ^ ": Error at line " ^ string_of_int l ^ " column " ^ string_of_int c
     ^ "\nClauses:" ^ ClauseMap.show clauses ^ "\n" ^ OccurrenceMap.show occur);
    assert false

let of_list =
  let rec aux
      ({ clauses; occur; unit_clauses = uc; two_literal_clauses = tlc; _ } as f)
      n = function
    | [] -> f
    | c :: cs ->
        let c = Clause.of_list c in
        let clauses' = ClauseMap.add n c clauses in
        let occur' =
          Clause.fold
            (fun l m ->
              OccurrenceMap.add_list_with
                ~f:(fun _ -> IntSet.union)
                m
                [ (l, IntSet.singleton n) ])
            c occur
        in
        let uc', tlc' =
          match Clause.size c with
          | 1 -> (ClauseMap.add n c uc, tlc)
          | 2 -> (uc, ClauseMap.add n c tlc)
          | _ -> (uc, tlc)
        in
        let f' =
          {
            f with
            clauses = clauses';
            original_clauses = clauses';
            unit_clauses = uc';
            two_literal_clauses = tlc';
            occur = occur';
          }
        in
        aux f' (n + 1) cs
  in
  aux
    {
      clauses = ClauseMap.empty;
      original_clauses = ClauseMap.empty;
      unit_clauses = ClauseMap.empty;
      two_literal_clauses = ClauseMap.empty;
      occur = OccurrenceMap.empty;
      decision_level = 0;
      assignments = Literal.Map.empty;
      trail = [];
      database = [];
    }
    1

let add_clause
    ({
       clauses;
       original_clauses;
       unit_clauses = uc;
       two_literal_clauses = tlc;
       occur;
       _;
     } as f) clause original_clause =
  (* let n = ClauseMap.size clauses in *)
  let n = ClauseMap.max_binding original_clauses |> fst in
  print_endline ("Adding clause: " ^ string_of_int (n + 1) ^ Clause.show clause);
  let clauses' = ClauseMap.add (n + 1) clause clauses in
  let original_clauses' =
    ClauseMap.add (n + 1) original_clause original_clauses
  in
  let occur' =
    Clause.fold
      (fun l m ->
        OccurrenceMap.add_list_with
          ~f:(fun _ -> IntSet.union)
          m
          [ (l, IntSet.singleton (n + 1)) ])
      clause occur
  in
  let uc', tlc' =
    match Clause.size clause with
    | 1 -> (ClauseMap.add (n + 1) clause uc, tlc)
    | 2 -> (uc, ClauseMap.add (n + 1) clause tlc)
    | _ -> (uc, tlc)
  in
  {
    f with
    clauses = clauses';
    original_clauses = original_clauses';
    unit_clauses = uc';
    two_literal_clauses = tlc';
    occur = occur';
  }

let show { clauses; occur; database; decision_level; trail; assignments; _ } =
  "Level: "
  ^ string_of_int decision_level
  ^ "\n" ^ "Clauses:" ^ ClauseMap.show clauses ^ "\nOccurrence map:"
  ^ OccurrenceMap.show occur ^ "\nDatabase:"
  ^ List.fold_left (fun s x -> s ^ " " ^ Clause.show x) "" database
  ^ "\nTrail:"
  ^ List.fold_left
      (fun s x ->
        s ^ " "
        ^
        match x with
        | Decision { literal = l; level = lv } ->
            "\t" ^ Literal.show l ^ " because of decision on level "
            ^ string_of_int lv ^ "\n"
        | Implication { literal = l; level = lv; implicant = i } ->
            "\t" ^ Literal.show l ^ " because of clause " ^ Clause.show i
            ^ " - implied at level " ^ string_of_int lv ^ "\n")
      "" (List.map fst trail)
  ^ "\nAssignments"
  ^ Literal.Map.fold
      (fun _ x s ->
        s ^ " "
        ^
        match x with
        | Decision { literal = l; level = lv } ->
            "\t" ^ Literal.show l ^ " because of decision on level "
            ^ string_of_int lv ^ "\n"
        | Implication { literal = l; level = lv; implicant = i } ->
            "\t" ^ Literal.show l ^ " because of clause " ^ Clause.show i
            ^ " - implied at level " ^ string_of_int lv ^ "\n")
      assignments ""

let add_learned_clauses ({ assignments = a; _ } as f) db =
  let clauses =
    List.filter_map
      (fun c ->
        let c' =
          Clause.fold
            (fun l c ->
              match Literal.(Map.get (var l) a) with
              | Some x -> (
                  match x with
                  | Decision { literal = l'; _ }
                  | Implication { literal = l'; _ } ->
                      if
                        not
                          (Bool.equal (Literal.is_negated l')
                             (Literal.is_negated l))
                      then Option.map (fun c -> Clause.remove l c) c
                      else None)
              | None -> c)
            c (Some c)
        in
        match c' with None -> None | Some c' -> Some (c', c))
      db
  in
  let f' = List.fold_left (fun f (c, oc) -> add_clause f c oc) f clauses in
  { f' with database = db }

let analyze_conflict { assignments = a; decision_level = d; _ } clause =
  let rec aux q c history =
    print_endline "State of queue: ";
    CCFQueue.iter (fun x -> print_string (Literal.show x ^ " ")) q;
    print_newline ();
    print_endline "History: ";
    Literal.Set.iter (fun x -> print_string (Literal.show x ^ " ")) history;
    print_newline ();
    match CCFQueue.take_front q with
    | None -> c
    | Some (l, q') -> (
        print_endline
          ("Seeing if " ^ Literal.show (Literal.var l) ^ " is assigned");
        if CCFQueue.size q = 1 then Clause.add l c
        else
          match Literal.Map.get (Literal.var l) a with
          | Some (Decision _) ->
              print_endline ("Adding " ^ Literal.show l ^ " to learned clause");
              aux q' (Clause.add l c) history
          | Some (Implication { implicant = ls; level = d'; _ }) ->
              if d' < d then (
                print_endline
                  ("Decision level " ^ string_of_int d' ^ " is less than "
                 ^ string_of_int d);
                print_endline ("Adding " ^ Literal.show l ^ " to learned clause");
                aux q' (Clause.add l c) history)
              else
                let ls = Clause.to_set ls in
                let ls' =
                  Literal.Set.filter
                    (fun l -> not Literal.(Set.mem (var l) history))
                    ls
                in
                let q'' =
                  Literal.Set.to_iter ls' |> CCFQueue.add_iter_back q'
                in
                let history' = Literal.(Set.union history (Set.map var ls')) in
                print_endline "Analyzing antecedents of variables: ";
                Literal.Set.iter
                  (fun x -> print_string (Literal.show x ^ " "))
                  ls;
                print_newline ();
                aux q'' c history'
          | _ ->
              print_endline
                ("fuck you: "
                ^ string_of_bool (Literal.Map.mem (Literal.var l) a));
              print_endline
                ("\nAssignments"
                ^ Literal.Map.fold
                    (fun _ x s ->
                      s ^ " "
                      ^
                      match x with
                      | Decision { literal = l; level = lv } ->
                          "\t" ^ Literal.show l
                          ^ " because of decision on level " ^ string_of_int lv
                          ^ "\n"
                      | Implication { literal = l; level = lv; implicant = i }
                        ->
                          "\t" ^ Literal.show l ^ " because of clause "
                          ^ Clause.show i ^ " - implied at level "
                          ^ string_of_int lv ^ "\n")
                    a "");
              aux q' c history)
  in
  print_endline
    ("Analyzing conflict for clause: " ^ Clause.show clause
   ^ " at decision level " ^ string_of_int d);
  let ls = Clause.to_list clause in
  aux (CCFQueue.of_list ls) Clause.empty
    (Literal.Set.map Literal.var (Clause.to_set clause))

let backtrack
    { assignments = a; trail = t; decision_level = d; database = db; _ }
    learned_clause =
  let d' =
    let open Iter in
    Clause.to_iter learned_clause
    |> filter_map (fun l -> Literal.(Map.get (var l) a))
    |> map (function Decision { level; _ } | Implication { level; _ } -> level)
    |> filter (fun level -> level < d)
    |> max |> Option.get_or ~default:0
  in
  let _, f =
    if d' = 0 then
      List.last_opt t
      |> Option.get_exn_or "Attempt to backtrack when trail is empty"
    else
      List.find_map
        (fun ((ass, _) as result) ->
          match ass with
          | Decision { level = d''; _ } ->
              if d'' = d' then Some result else None
          | _ -> None)
        t
      |> Option.get_exn_or "Attempt to backtrack when trail is empty"
  in
  print_endline "Adding database to formula:";
  List.iter (fun x -> print_string (Clause.show x ^ " ")) (learned_clause :: db);
  let f' = add_learned_clauses f (learned_clause :: db) in
  print_endline ("Result after adding: " ^ show f');
  (f', d')

let choose_literal { occur; two_literal_clauses = tlc; _ } =
  ClauseMap.choose tlc |> snd |> Clause.choose
(* let vs = *)
(*   ClauseMap.fold *)
(*     (fun _ ls x -> Literal.Set.union x (Clause.to_set ls)) *)
(*     tlc Literal.Set.empty *)
(* in *)
(* let v = *)
(*   let open Iter in *)
(*   (if ClauseMap.is_empty tlc then OccurrenceMap.to_iter occur *)
(*    else *)
(*      OccurrenceMap.to_iter occur *)
(*      |> filter (fun (k, _) -> Literal.Set.mem k vs)) *)
(*   |> filter_map (fun (k, v) -> *)
(*          if Literal.is_negated k then None *)
(*          else *)
(*            let pos = v in *)
(*            let neg = *)
(*              OccurrenceMap.get_or ~default:IntSet.empty (Literal.neg k) occur *)
(*            in *)
(*            Some (k, IntSet.union pos neg)) *)
(*   |> max ~lt:(fun (_, v1) (_, v2) -> *)
(*          compare (IntSet.cardinal v1) (IntSet.cardinal v2) < 0) *)
(*   |> Option.get_exn_or "HAHA" |> fst *)
(* in *)
(* let sz1 = *)
(*   OccurrenceMap.get v occur |> Option.map_or ~default:0 IntSet.cardinal *)
(* in *)
(* let sz2 = *)
(*   OccurrenceMap.get (Literal.neg v) occur *)
(*   |> Option.map_or ~default:0 IntSet.cardinal *)
(* in *)
(* if sz1 > sz2 then v else Literal.neg v *)

let is_empty { clauses; _ } = ClauseMap.is_empty clauses

let restart ({ trail = t; database = db; _ } as f) =
  if List.is_empty t then f
  else
    let f' =
      List.last_opt t |> Option.map snd |> Option.get_exn_or "Impossible"
    in
    add_learned_clauses f' db

let simplify ({ occur; _ } as f) l =
  let delete_literal l ({ occur; original_clauses = oc; _ } as f) =
    try
      match OccurrenceMap.get l occur with
      | None -> Ok f
      | Some cs ->
          let f' =
            IntSet.fold
              (fun c
                   ({ clauses; unit_clauses = uc; two_literal_clauses = tlc; _ }
                   as f') ->
                match ClauseMap.get c clauses with
                | None -> f'
                | Some ls -> (
                    let diff = Clause.remove l ls in
                    let clauses' = ClauseMap.add c diff clauses in
                    match Clause.size diff with
                    | 0 ->
                        raise_notrace
                          (ClauseMap.Empty_clause (ClauseMap.find c oc))
                    | 1 ->
                        {
                          f' with
                          clauses = clauses';
                          unit_clauses = ClauseMap.add c diff uc;
                          two_literal_clauses = ClauseMap.remove c tlc;
                        }
                    | 2 ->
                        {
                          f' with
                          clauses = clauses';
                          two_literal_clauses = ClauseMap.add c diff tlc;
                        }
                    | _ -> { f' with clauses = clauses' }))
              cs f
          in
          Ok { f' with occur = OccurrenceMap.remove l occur }
    with ClauseMap.Empty_clause ls -> Error (ls, f)
  in
  let delete_clauses =
    IntSet.fold
      (fun
        c
        ({ clauses; occur; unit_clauses = uc; two_literal_clauses = tlc; _ } as
        f)
      ->
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
        let uc' = ClauseMap.remove c uc in
        let tlc' = ClauseMap.remove c tlc in
        {
          f with
          clauses = clauses';
          occur = occur';
          unit_clauses = uc';
          two_literal_clauses = tlc';
        })
  in
  print_endline ("Simplifying by : " ^ Literal.show l);
  delete_literal (Literal.neg l) f
  |> Result.map (fun f' ->
         print_endline
           ("Result after removing "
           ^ Literal.show (Literal.neg l)
           ^ ": " ^ show f');
         match OccurrenceMap.get l occur with
         | None ->
             print_endline
               ("Literal " ^ Literal.show l
              ^ "not present, returning formula unchanged");
             f'
         | Some cs ->
             print_endline
               ("Clauses that " ^ Literal.show l ^ " appears in: "
              ^ IntSet.show cs);
             let ({ occur; _ } as f'') = delete_clauses cs f' in
             print_endline
               ("Result after removing clauses: " ^ IntSet.show cs ^ ": "
              ^ show f'');
             { f'' with occur = OccurrenceMap.remove l occur })

let rewrite ({ decision_level = d; assignments = a; trail = t; _ } as f) l =
  print_endline ("Rewriting by : " ^ Literal.show l);
  let f' = simplify f l |> Result.get_exn in
  print_endline ("Result after simplifying: " ^ show f');
  let a' =
    Literal.(Map.add (var l) (Decision { literal = l; level = d + 1 }) a)
  in
  let t' = (Decision { literal = l; level = d + 1 }, f) :: t in
  { f' with decision_level = d + 1; assignments = a'; trail = t' }

let rec unit_propagate
    ({ assignments = a; unit_clauses = uc; original_clauses = oc; trail = t; _ }
    as f) =
  print_endline "Unit propagating: ";
  match ClauseMap.choose_opt uc with
  | None ->
      print_endline "Nothing to propagate";
      Ok f
  | Some (c, ls) ->
      print_endline (show f);
      let l = Clause.choose ls in
      let ls = ClauseMap.find c oc in
      let d =
        let open Iter in
        Clause.remove l ls |> Clause.to_iter
        |> filter_map (fun l -> Literal.(Map.get (var l) a))
        |> map (function Decision { level; _ } | Implication { level; _ } ->
               level)
        |> max |> Option.get_or ~default:0
      in
      let i = Implication { literal = l; implicant = ls; level = d } in
      let a' = Literal.(Map.add (var l) i a) in
      let t' = (i, f) :: t in
      let f' = { f with assignments = a'; trail = t' } in
      simplify f' l |> Result.flat_map unit_propagate
