include List

type elt = Literal.t
type t = { set : elt list; size : int; present : Z.t }

let find_first_unset { set; present; _ } =
  let exception Found of Literal.t in
  try
    fold_left
      (fun a l ->
        let open Z in
        if equal (a land one) zero then raise_notrace (Found l)
        else shift_right_trunc a 1)
      present set
    |> ignore;
    raise Not_found
  with Found l -> l

let reify { set; present; _ } =
  let open Z in
  fold_left
    (fun (c, a) l ->
      let a' = shift_right_trunc a 1 in
      if equal (a land one) zero then (l :: c, a') else (c, a'))
    ([], present) set
  |> fst

let add l { set; present; size } =
  match List.find_opt (fun l' -> l' = l) set with
  | None -> { set = l :: set; size = size + 1; present = Z.(present lsl 1) }
  | Some _ -> { set; present; size }

let choose = find_first_unset
let empty = { set = empty; size = 0; present = Z.zero }
let fold f c acc = reify c |> fold_left (Fun.flip f) acc
let is_empty { size; present; _ } = Z.popcount present = size
let mem l { set; _ } = mem l set

let of_list ls =
  let set = Iter.(of_list ls |> map Literal.of_int |> sort_uniq |> of_iter) in
  { set; size = length set; present = Z.zero }

let original { set; size; _ } = { set; size; present = Z.zero }

let remove l { set; size; present } =
  let idx =
    List.find_idx (fun l' -> l' = l) set |> Option.get_exn_or "IDX" |> fst
  in
  { set; size; present = Z.(present lor (one lsl idx)) }

let singleton x = { set = [ x ]; size = 1; present = Z.zero }
let size { size; present; _ } = size - Z.popcount present

let show c =
  "("
  ^ fold_left
      (fun s l -> Printf.sprintf "%s%s " s (Literal.show l))
      "" (reify c)
  ^ ")"

let to_iter c = reify c |> to_iter
let to_list = reify
let to_set c = reify c |> Literal.Set.of_list
