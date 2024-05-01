include CCArray

type t = int * Literal.t array * int * int

type clause = t

(* let _lbd (_, _c) _a = 0 *)

let equal (id1, _, _, _) (id2, _, _, _) = id1 = id2

let hash (id, _, _, _) = id

let of_list id l =
  let a = of_list l in
  (id, a, 0, length a)

let of_iter id iterator = Iter.to_list iterator |> of_list id

(* let size (_, c,_,_) = length c *)
let size (_, c, _, len) = len

let show (_, c, _, _) =
  fold (fun s l -> Printf.sprintf "%s%s " s (Literal.show l)) "" c

let to_array (_, c, _, _) = c

(* let to_iter (_, c) = to_iter c *)
let to_iter (_, c, idx, len) f =
  for i = idx to idx + len - 1 do
    f (unsafe_get c i)
  done

module Set = struct
  include CCHashSet.Make (struct
    type t = clause

    let equal (id1, _, _, _) (id2, _, _, _) = id1 = id2

    let hash (id, _, _, _) = id
  end)

  let add c s = insert s c ; s

  (* TODO *)
  let empty () = create 100

  let remove c s = remove s c ; s

  let show s = fold (fun s c -> Printf.sprintf "%s:%s\n" s (show c)) "" s
end
