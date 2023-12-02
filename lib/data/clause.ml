include List

type elt = Literal.t
type t = { clause : elt list; assigned : Z.t }

let find_first_unassigned { clause; assigned } =
  let exception Found of Literal.t in
  try
    fold_left
      (fun a l ->
        let open Z in
        if equal (logand a one) zero then raise_notrace (Found l)
        else shift_right a 1)
      assigned clause
    |> ignore;
    raise Not_found
  with Found l -> l

let reify { clause; assigned; _ } =
  let open Z in
  fold_left
    (fun (c, a) l ->
      let a' = shift_right a 1 in
      if equal (logand a one) zero then (l :: c, a') else (c, a'))
    ([], assigned) clause
  |> fst

let add l { clause; assigned } =
  let open Z in
  match List.find_opt (fun l' -> l' = l) clause with
  | None -> { clause = l :: clause; assigned = shift_left assigned 1 }
  | Some _ -> { clause; assigned }

let choose = find_first_unassigned
let empty = { clause = empty; assigned = Z.zero }
let fold f c acc = reify c |> fold_left (Fun.flip f) acc
let is_empty { clause; assigned } = Z.popcount assigned = length clause
let mem l { clause; _ } = mem l clause

let of_list ls =
  let clause =
    Iter.(of_list ls |> map Literal.of_int |> sort_uniq |> of_iter)
  in
  { clause; assigned = Z.zero }

let original { clause; _ } = { clause; assigned = Z.zero }

let remove l { clause; assigned } =
  let idx =
    List.find_idx (fun l' -> l' = l) clause |> Option.get_exn_or "IDX" |> fst
  in
  let open Z in
  { clause; assigned = logor assigned (shift_left one idx) }

let size { clause; assigned; _ } = length clause - Z.popcount assigned

let show c =
  "("
  ^ fold_left
      (fun s l -> Printf.sprintf "%s%s " s (Literal.show l))
      "" (reify c)
  ^ ")"

let to_iter c = reify c |> to_iter
let to_list = reify
let to_set c = reify c |> Literal.Set.of_list
