open Common
include IntMap

type t = Clause.t IntMap.t

(* let count = ref 0 *)
(* TODO *)

(* let add c m = *)
let add n c m =
  (* incr count; *)
  (* add !count *)
  (* add (cardinal m + 1) c m *)
  add n c m

let remove_literal_from_clauses l cs m =
  let exception Empty_clause of Clause.t in
  try
    Ok
      (IntSet.fold
         (fun c (m', os) ->
           match get c m' with
           | None -> (m', os)
           | Some ls -> (
               let diff = Clause.remove l ls in
               (* print_endline ("Diff:" ^ Clause.show diff); *)
               let occurrences = Clause.make_occurrences diff c in
               match Clause.size diff with
               | 0 -> raise_notrace (Empty_clause ls)
               | _ -> (IntMap.add c diff m', occurrences @ os)))
         cs (m, []))
  with Empty_clause ls -> Error ls

let remove_many = IntSet.fold (fun c m' -> remove c m')
let size m = max_binding_opt m |> Option.map_or ~default:0 fst

let show m =
  fold
    (fun _ v s -> (if String.equal s "" then "" else s ^ "/\\") ^ Clause.show v)
    m ""
