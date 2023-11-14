open Common
include IntMap

type t = Clause.t IntMap.t

(* let count = ref 0 *)
(* TODO *)

let add c m =
  (* incr count; *)
  (* add !count *)
  add (cardinal m + 1) c m

let remove_literal_from_clauses l cs m =
  assert (Literal.is_negated l);
  let exception Empty_clause of Clause.t in
  try
    Ok
      (IntSet.fold
         (fun c (m', os) ->
           match get c m' with
           | None -> (m', os)
           | Some ls -> (
               let diff = Clause.remove l ls in
               let occurrences = Clause.make_occurrences diff c in
               match Clause.size diff with
               | 0 -> raise_notrace (Empty_clause ls)
               | _ -> (IntMap.add c diff m', occurrences @ os)))
         cs (m, []))
  with Empty_clause ls -> Error ls

let remove_many = IntSet.fold (fun c m' -> remove c m')
