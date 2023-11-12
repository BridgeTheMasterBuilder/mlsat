open Containers

module Literal : sig
  type t

  val compare : t -> t -> int
  val invalid : t
  val is_negated : t -> bool
  val neg : t -> t
  val of_int : int -> t
  val var : t -> t
end = struct
  include Int

  let invalid = 0
  let is_negated l = l < 0
  let of_int i = if i = 0 then failwith "Invalid literal" else i
  let var = abs
end

module LiteralSet = Iter.Set.Make (Literal)

module Clause : sig
  type t
  type elt = Literal.t

  val make_occurrences : t -> int -> (Literal.t * int) list
  val of_list : int list -> t
  val remove : elt -> t -> t
  val size : t -> int
end = struct
  include LiteralSet

  let make_occurrences ls c =
    Iter.(to_iter ls |> map (fun x -> (x, c)) |> to_list)

  let of_list ls = Iter.(of_list ls |> map Literal.of_int |> of_iter)
  let size = cardinal
end

module IntMap = Map.Make (Int)
module IntSet = Iter.Set.Make (Int)

module ClauseMap : sig
  type t
  type key = int

  val add : Clause.t -> t -> t
  val empty : t
  val find : key -> t -> Clause.t
  val is_empty : t -> bool

  val remove_literal_from_clauses :
    Clause.elt -> IntSet.t -> t -> (t, Clause.t) result
end = struct
  include IntMap

  type t = Clause.t IntMap.t

  let count = ref 0
  (* TODO *)

  let add =
    incr count;
    add !count

  (* TODO return list of occurrences in order to update occurrence map *)
  let remove_literal_from_clauses l cs m =
    let exception Empty_clause of Clause.t in
    try
      Ok
        (IntSet.fold
           (fun c m' ->
             match get c m' with
             | None -> m'
             | Some ls -> (
                 let diff = Clause.remove l ls in
                 match Clause.size diff with
                 | 0 -> raise_notrace (Empty_clause ls)
                 | _ -> IntMap.add c diff m'))
           cs m)
    with Empty_clause ls -> Error ls
end

module OccurrenceMap : sig
  type t
  type key = Literal.t

  val add_occurrences : t -> (key * int) list -> t
  val choose_opt : t -> (key * int) option
  val empty : t
  val fold : (key -> IntSet.t * IntSet.t -> 'a -> 'a) -> t -> 'a -> 'a
  val get : key -> t -> IntSet.t option
  val is_empty : t -> bool
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
  (* TODO inline add_occurrence *)

  let choose_opt m =
    choose_opt m
    |> Option.map (fun (l, (pos, neg)) ->
           if IntSet.cardinal pos > 1 then (Literal.neg l, IntSet.choose neg)
           else (l, IntSet.choose pos))

  let get l m =
    get l m
    |> Option.map (fun (pos, neg) -> if Literal.is_negated l then neg else pos)
end

type assignment =
  | Decision of { literal : Literal.t; level : int }
  | Implication of { literal : Literal.t; level : int; implicant : Clause.t }

module LiteralMap = Map.Make (Literal)

type occurrence_map = {
  occur1 : OccurrenceMap.t;
  occur2 : OccurrenceMap.t;
  occur_many : OccurrenceMap.t;
}

type formula = {
  clauses : ClauseMap.t;
  original_clauses : ClauseMap.t;
  occur : occurrence_map;
  decision_level : int;
  assignments : assignment LiteralMap.t;
  trail : (assignment * formula) list;
  database : IntSet.t list;
}

let of_list =
  let rec aux
      ({ clauses; occur = { occur1 = o1; occur2 = o2; occur_many = om }; _ } as
      f) n = function
    | [] -> f
    | c :: cs ->
        let c = Clause.of_list c in
        let clauses' = ClauseMap.add c clauses in
        let occurrences = Clause.make_occurrences c n in
        let o1', o2' =
          match Clause.size c with
          | 1 -> (OccurrenceMap.add_occurrences o1 occurrences, o2)
          | 2 -> (o1, OccurrenceMap.add_occurrences o2 occurrences)
          | _ -> (o1, o2)
        in
        let om' = OccurrenceMap.add_occurrences om occurrences in
        let f' =
          {
            f with
            clauses = clauses';
            original_clauses = clauses';
            occur = { occur1 = o1'; occur2 = o2'; occur_many = om' };
          }
        in
        aux f' (n + 1) cs
  in
  aux
    {
      clauses = ClauseMap.empty;
      original_clauses = ClauseMap.empty;
      occur =
        {
          occur1 = OccurrenceMap.empty;
          occur2 = OccurrenceMap.empty;
          occur_many = OccurrenceMap.empty;
        };
      decision_level = 0;
      assignments = LiteralMap.empty;
      trail = [];
      database = [];
    }
    1

let is_empty { clauses; _ } = ClauseMap.is_empty clauses

let choose_literal { occur = { occur2 = o2; occur_many = om; _ }; _ } =
  let m = if OccurrenceMap.is_empty o2 then om else o2 in
  OccurrenceMap.fold
    (fun l (pos, neg) (l', m) ->
      let size_pos = IntSet.cardinal pos in
      let size_neg = IntSet.cardinal neg in
      let occurrences = size_pos + size_neg in
      let l = if size_pos < size_neg then Literal.(neg l) else l in
      if occurrences > m then (l, occurrences) else (l', m))
    m (Literal.invalid, 0)
  |> fst

let delete_literal f l = Ok f
let raw_delete_literal l f = f
let delete_clauses f c = f

let simplify ({ occur = { occur_many = om; _ }; _ } as f) l =
  delete_literal f (Literal.neg l)
  |> Result.map (fun f' ->
         match OccurrenceMap.get l om with
         | None -> f'
         | Some cs -> delete_clauses f' cs |> raw_delete_literal l)

let rec unit_propagate
    ({
       assignments = a;
       original_clauses = oc;
       trail = t;
       occur = { occur1 = o1; _ };
       _;
     } as f) =
  match OccurrenceMap.choose_opt o1 with
  | Some (l, c) ->
      let ls = ClauseMap.find c oc in
      let d = 0 in
      (* TODO *)
      let i = Implication { literal = l; implicant = ls; level = d } in
      let a' = LiteralMap.add (Literal.var l) i a in
      let t' = (i, f) :: t in
      let f' = { f with assignments = a'; trail = t' } in
      simplify f' l |> Result.flat_map (fun f'' -> unit_propagate f'')
  | None -> Ok f
