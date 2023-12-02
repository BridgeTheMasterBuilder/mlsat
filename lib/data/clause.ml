(* include Literal.Set *)
include List

type elt = Literal.t
type t = { clause : elt list; assigned : int }

let show_ clause =
  "("
  ^ fold_left (fun s l -> Printf.sprintf "%s%s " s (Literal.show l)) "" clause
  ^ ")"
(* TODO assigned *)

let find_first_unassigned { clause; assigned } =
  (* Printf.printf "Clause: %s, assignments: %d\n" (show_ clause) assigned; *)
  let exception Found of Literal.t in
  try
    fold_left
      (fun a l -> if a land 1 = 0 then raise_notrace (Found l) else a lsr 1)
      assigned clause
    |> ignore;
    raise Not_found
  with Found l -> (* Printf.printf "Found %s\n" (Literal.show l); *)
                  l

let reify { clause; assigned; _ } =
  fold_left
    (fun (c, a) l ->
      let a' = a lsr 1 in
      if a land 1 = 0 then (l :: c, a') else (c, a'))
    ([], assigned) clause
  |> fst

let add l { clause; assigned } =
  { clause = l :: clause; assigned = assigned lsl 1 }

let choose = find_first_unassigned
let empty = { clause = empty; assigned = 0 }
let fold f c acc = reify c |> fold_left (Fun.flip f) acc

let is_empty { clause; assigned } =
  Z.(of_int assigned |> popcount) = length clause

let mem l { clause; _ } = mem l clause

let of_list ls =
  let clause = Iter.(of_list ls |> map Literal.of_int |> of_iter) in
  { clause; assigned = 0 }

let original { clause; _ } = { clause; assigned = 0 }

let remove l { clause; assigned } =
  let idx =
    List.find_idx (fun l' -> l' = l) clause |> Option.get_exn_or "IDX" |> fst
  in
  { clause; assigned = assigned lor (1 lsl idx) }

let size { clause; assigned; _ } =
  length clause - Z.(of_int assigned |> popcount)

let show { clause; _ } = show_ clause
let to_iter c = reify c |> to_iter
let to_list = reify
let to_set c = reify c |> Literal.Set.of_list
