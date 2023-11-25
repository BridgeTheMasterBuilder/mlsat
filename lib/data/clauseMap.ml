open Common
include IntMap

type t = Clause.t IntMap.t

exception Empty_clause of Clause.t

(* let count = ref 0 *)
(* TODO *)

(* let add c m = *)
let add n c m =
  (* incr count; *)
  (* add !count *)
  (* add (cardinal m + 1) c m *)
  add n c m

(* let choose m = choose m |> fst *)
let choose m = choose m

let remove_literal_from_clauses l cs m =
  try
    Ok
      (IntSet.fold
         (fun c (m', os) ->
           match get c m' with
           (* TODO find *)
           | None -> (m', os)
           | Some ls -> (
               let diff = Clause.remove l ls in
               (* print_endline ("Diff:" ^ Clause.show diff); *)
               let occurrences = Clause.make_occurrences diff c in
               (* TODO inline make_occurrences *)
               match Clause.size diff with
               | 0 -> raise_notrace (Empty_clause ls)
               | _ -> (IntMap.add c diff m', occurrences @ os)))
         cs (m, []))
  with Empty_clause ls -> Error ls

let remove_many cs m =
  IntSet.fold
    (fun c (m', rem) ->
      let ls = find c m' in
      let rem' =
        Clause.fold
          (fun l rem' ->
            print_endline
              ("Need to delete " ^ string_of_int c ^ " from " ^ Literal.show l);
            (l, c) :: rem')
          ls rem
      in
      (remove c m', rem'))
    cs (m, [])

let size m = max_binding_opt m |> Option.map_or ~default:0 fst

(* let show m = *)
(*   fold *)
(*     (fun c v s -> *)
(*       (if String.equal s "" then "" else s ^ "/\\") *)
(*       ^ string_of_int c ^ ":" ^ Clause.show v) *)
(*     m "" *)
