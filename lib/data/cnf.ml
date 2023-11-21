open Common

type assignment =
  | Decision of { literal : Literal.t; level : int }
  | Implication of { literal : Literal.t; level : int; implicant : Clause.t }

module ClauseMap = struct
  include IntMap

  type t = Clause.t IntMap.t

  let size = cardinal
end

module OccurrenceMap = struct
  include Literal.Map

  type t = IntSet.t Literal.Map.t
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

let of_list =
  let rec aux ({ clauses; occur; _ } as f) n = function
    | [] -> f
    | c :: cs ->
        let c = Clause.of_list c in
        let clauses' = ClauseMap.add n c clauses in
        let occur' =
          Clause.fold
            (fun l m -> OccurrenceMap.add l (IntSet.singleton n) m)
            c occur
        in
        let f' =
          {
            f with
            clauses = clauses';
            original_clauses = clauses';
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

let add_clause ({ clauses; original_clauses; occur; _ } as f) clause
    original_clause =
  let n = ClauseMap.size clauses in
  let clauses' = ClauseMap.add (n + 1) clause clauses in
  let original_clauses' =
    ClauseMap.add (n + 1) original_clause original_clauses
  in
  let occur' =
    Clause.fold
      (fun l m -> OccurrenceMap.add l (IntSet.singleton (n + 1)) m)
      clause occur
  in
  {
    f with
    clauses = clauses';
    original_clauses = original_clauses';
    occur = occur';
  }

let add_learned_clauses ({ assignments = a; _ } as f) db =
  let clauses =
    List.map
      (fun c ->
        let c' =
          Clause.fold
            (fun l c ->
              Literal.(Map.get (var l) a)
              |> Option.map (function
                     | Decision { literal = l'; _ }
                     | Implication { literal = l'; _ }
                     ->
                     if
                       Bool.equal (Literal.is_negated l') (Literal.is_negated l)
                     then Clause.remove l c
                     else c)
              |> Option.get_exn_or "foo")
            c c
        in
        (c', c))
      db
  in
  let f' = List.fold_left (fun f (c, oc) -> add_clause f c oc) f clauses in
  { f' with database = db }

let analyze_conflict { assignments = a; decision_level = d; _ } clause =
  let rec aux q c history =
    match CCFQueue.take_front q with
    | None -> c
    | Some (l, q') -> (
        if CCFQueue.size q = 1 then Clause.add l c
        else
          match Literal.Map.get l a with
          | Some (Decision _) -> aux q' (Clause.add l c) history
          | Some (Implication { implicant = ls; level = d'; _ }) ->
              if d' < d then aux q' (Clause.add l c) history
              else
                let ls = Clause.to_set ls in
                let ls' =
                  Literal.Set.filter
                    (fun l -> not Literal.(Set.mem (var l) history))
                    ls
                in
                let q'' = Literal.Set.to_iter ls |> CCFQueue.add_iter_back q' in
                let history' = Literal.(Set.union history (Set.map var ls')) in
                aux q'' c history'
          | _ -> aux q' c history)
  in
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
  let f' = add_learned_clauses f (learned_clause :: db) in
  (f', d')

let choose_literal { clauses; two_literal_clauses = tlc; _ } =
  (* let m = if OccurrenceMap.is_empty o2 then om else o2 in *)
  (* OccurrenceMap.fold *)
  (*   (fun l (pos, neg) (l', m) -> *)
  (*     let size_pos = IntSet.cardinal pos in *)
  (*     let size_neg = IntSet.cardinal neg in *)
  (*     let occurrences = size_pos + size_neg in *)
  (*     let l = if size_pos < size_neg then Literal.(neg l) else l in *)
  (*     if occurrences > m then (l, occurrences) else (l', m)) *)
  (*   m (Literal.invalid, 0) *)
  (* |> fst *)
  Literal.of_int 1 (* TODO *)

let is_empty { clauses; _ } = ClauseMap.is_empty clauses

let restart ({ trail = t; database = db; _ } as f) =
  if List.is_empty t then f
  else
    let f' =
      List.last_opt t |> Option.map snd |> Option.get_exn_or "Impossible"
    in
    add_learned_clauses f' db

let simplify ({ clauses; occur; _ } as f) l = Ok f
(* print_endline ("Simplifying by : " ^ Literal.show l); *)
(* match OccurrenceMap.get l om with *)
(* | None -> *)
(*     (\* f *\) failwith "Attempt to simplify clause by non-existent literal" *)
(* | Some (pos, neg) -> *)
(*     print_endline *)
(*       ("Clauses that " ^ Literal.show l ^ " appears in: " *)
(*       ^ IntSet.show (IntSet.union pos neg)); *)
(*     let pos, neg = if Literal.is_negated l then (neg, pos) else (pos, neg) in *)
(*     ClauseMap.remove_literal_from_clauses (Literal.neg l) neg clauses *)
(*     |> Result.map (fun (clauses', os) -> *)
(*            let clauses', rem = ClauseMap.remove_many pos clauses' in *)
(*            (\* TODO generate occurrence map here too *\) *)
(*            let occur' = OccurrenceMap.remove_occurrences occur rem in *)
(*            let occur' = OccurrenceMap.remove l occur' in *)
(*            let occur' = OccurrenceMap.update_occurrences occur' os in *)
(*            { f with clauses = clauses'; occur = occur' }) *)

let rewrite ({ decision_level = d; assignments = a; trail = t; _ } as f) l =
  print_endline ("Rewriting by : " ^ Literal.show l);
  let f' = simplify f l |> Result.get_exn in
  let a' =
    Literal.(Map.add (var l) (Decision { literal = l; level = d + 1 }) a)
  in
  let t' = (Decision { literal = l; level = d + 1 }, f) :: t in
  { f' with decision_level = d + 1; assignments = a'; trail = t' }

let unit_propagate
    ({ assignments = a; original_clauses = oc; trail = t; occur; _ } as f) =
  Ok f
(* print_endline ("Unit propagating: " ^ show f); *)

(* match OccurrenceMap.choose_opt o1 with *)
(* | Some (l, c) -> *)
(*     let ls = ClauseMap.find c oc in *)
(*     let d = *)
(*       let open Iter in *)
(*       Clause.remove l ls |> Clause.to_iter *)
(*       |> map (fun l -> *)
(*              Literal.(Map.get (var l) a) *)
(*              |> Option.map_or ~default:0 (function *)
(*                     | Decision { level; _ } | Implication { level; _ } -> *)
(*                     level)) *)
(*       |> max |> Option.get_or ~default:0 *)
(*     in *)
(*     let i = Implication { literal = l; implicant = ls; level = d } in *)
(*     let a' = Literal.(Map.add (var l) i a) in *)
(*     let t' = (i, f) :: t in *)
(*     let f' = { f with assignments = a'; trail = t' } in *)
(*     simplify f' l *)
(*     |> Result.flat_map (fun f'' -> *)
(*            print_endline (show f''); *)
(*            check_invariants f''; *)
(*            unit_propagate f'') *)
(* | None -> *)
(*     print_endline "Nothing to propagate"; *)
(*     Ok f *)
