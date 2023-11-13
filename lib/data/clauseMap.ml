open Common
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
