include CCArray

type t = int * Literal.t array

type clause = t

(* let _lbd (_, _c) _a = 0 *)

let equal (id1, _) (id2, _) = id1 = id2

let hash (id, _) = id

let of_list id l =
  let a = of_list l in
  (id, a)

let of_iter id iterator = Iter.to_list iterator |> of_list id

let size (_, c) = length c

let show (_, c) =
  fold (fun s l -> Printf.sprintf "%s%s " s (Literal.show l)) "" c

let to_array (_, c) = c

let to_iter (_, c) = to_iter c

module Set = struct
  include CCHashSet.Make (struct
    type t = clause

    let equal (id1, _) (id2, _) = id1 = id2

    let hash (id, _) = id
  end)

  let add c s = insert s c ; s

  (* TODO *)
  let empty () = create 100

  let remove c s = remove s c ; s

  let show s = fold (fun s c -> Printf.sprintf "%s:%s\n" s (show c)) "" s
end
