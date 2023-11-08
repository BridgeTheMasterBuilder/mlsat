open Containers

module Literal : sig
  type t

  val compare : t -> t -> int
  val var : t -> t
  val neg : t -> t
  val is_negated : t -> bool
  val invalid : t
  val of_int : int -> t
end = struct
  include Int

  let var = abs
  let is_negated l = l < 0
  let invalid = 0
  let of_int i = if i = 0 then failwith "Invalid literal" else i
end

module IntSet = Iter.Set.Make (Int)

module Clause : sig
  type t

  val of_list : int list -> t
  val make_occurrences : t -> int -> (Literal.t * int) list
  val size : t -> int
end = struct
  include IntSet

  let make_occurrences ls c =
    Iter.(to_iter ls |> map (fun x -> (Literal.of_int x, c)) |> to_list)

  let size = cardinal
end

module IntMap = Map.Make (Int)

module ClauseMap : sig
  type t
  type key = int

  val add_clause : key -> Clause.t -> t -> t
  val empty : t
  val find : key -> t -> Clause.t
  val is_empty : t -> bool
end = struct
  include IntMap

  type t = Clause.t IntMap.t

  let add_clause = add
end

module OccurrenceMap : sig
  type t
  type key = Literal.t

  val add_occurrences : t -> (key * int) list -> t
  val choose_opt : t -> (key * int) option
  val empty : t
  val get : key -> t -> IntSet.t option
  val is_empty : t -> bool
  val fold : (key -> IntSet.t * IntSet.t -> 'a -> 'a) -> t -> 'a -> 'a
end = struct
  module S = Map.Make (Literal)
  include S

  type key = Literal.t
  type t = (IntSet.t * IntSet.t) S.t

  let add_occurrence l n m =
    let map, select, occurrence =
      if Literal.is_negated l then
        (Pair.map_snd, snd, (Literal.var l, (IntSet.empty, IntSet.singleton n)))
      else
        (Pair.map_fst, fst, (Literal.var l, (IntSet.singleton n, IntSet.empty)))
    in
    add_list_with ~f:(fun _ o -> map (IntSet.union (select o))) m [ occurrence ]

  let add_occurrences = List.fold_left (fun m (l, c) -> add_occurrence l c m)

  let get l m =
    get l m
    |> Option.map (fun (pos, neg) -> if Literal.is_negated l then neg else pos)

  let choose_opt m =
    choose_opt m
    |> Option.map (fun (l, (pos, neg)) ->
           if IntSet.cardinal pos > 1 then (Literal.neg l, IntSet.choose neg)
           else (l, IntSet.choose pos))
end

type assignment =
  | Decision of { literal : Literal.t; level : int }
  | Implication of { literal : Literal.t; level : int; implicant : Clause.t }

module LiteralMap = Map.Make (Literal)

type formula = {
  clauses : ClauseMap.t;
  original_clauses : ClauseMap.t;
  occurrence_map_1 : OccurrenceMap.t;
  occurrence_map_2 : OccurrenceMap.t;
  occurrence_map_n : OccurrenceMap.t;
  decision_level : int;
  assignments : assignment LiteralMap.t;
  trail : (assignment * formula) list;
  database : IntSet.t list;
}

let of_list =
  let rec aux
      ({
         clauses;
         occurrence_map_1 = uc;
         occurrence_map_2 = tlc;
         occurrence_map_n = om;
         _;
       } as f) n = function
    | [] -> f
    | c :: cs ->
        let c = Clause.of_list c in
        let clauses' = ClauseMap.add_clause n c clauses in
        let occurrences = Clause.make_occurrences c n in
        let uc', tlc' =
          match Clause.size c with
          | 1 -> (OccurrenceMap.add_occurrences uc occurrences, tlc)
          | 2 -> (uc, OccurrenceMap.add_occurrences tlc occurrences)
          | _ -> (uc, tlc)
        in
        let om' = OccurrenceMap.add_occurrences om occurrences in
        let f' =
          {
            f with
            clauses = clauses';
            original_clauses = clauses';
            occurrence_map_1 = uc';
            occurrence_map_2 = tlc';
            occurrence_map_n = om';
          }
        in
        aux f' (n + 1) cs
  in
  aux
    {
      clauses = ClauseMap.empty;
      original_clauses = ClauseMap.empty;
      occurrence_map_1 = OccurrenceMap.empty;
      occurrence_map_2 = OccurrenceMap.empty;
      occurrence_map_n = OccurrenceMap.empty;
      decision_level = 0;
      assignments = LiteralMap.empty;
      trail = [];
      database = [];
    }
    1

let is_empty { clauses; _ } = ClauseMap.is_empty clauses

let choose_literal { occurrence_map_2 = tlc; occurrence_map_n = om; _ } =
  let m = if OccurrenceMap.is_empty tlc then om else tlc in
  OccurrenceMap.fold
    (fun l (pos, neg) (l', m) ->
      let size_pos = IntSet.cardinal pos in
      let size_neg = IntSet.cardinal neg in
      let occurrences = size_pos + size_neg in
      let l = if size_pos < size_neg then Literal.(neg l) else l in
      if occurrences > m then (l, occurrences) else (l', m))
    m (Literal.invalid, 0)
  |> fst

let simplify ({ occurrence_map_n = om; _ } as f) l =
  delete_literal f (Literal.neg l)
  |> Result.map (fun f' ->
         match OccurrenceMap.get l om with
         | None -> Ok f'
         | Some cs -> delete_clauses f' cs |> raw_delete_literal l)

let rec unit_propagate
    ({
       assignments = a;
       original_clauses = oc;
       trail = t;
       occurrence_map_1 = uc;
       _;
     } as f) =
  match OccurrenceMap.choose_opt uc with
  | Some (l, c) ->
      let ls = ClauseMap.find c oc in
      let d = 0 in
      let i = Implication { literal = l; implicant = ls; level = d } in
      let a' = LiteralMap.add (Literal.var l) i a in
      let t' = (i, f) :: t in
      let f' = { f with assignments = a'; trail = t' } in
      simplify f' l |> Result.flat_map (fun f'' -> unit_propagate f'')
  | None -> Ok f
