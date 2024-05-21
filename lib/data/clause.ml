include CCArray

type t = int * Literal.t array

type clause = t

let compare (id1, _) (id2, _) = Int.compare id1 id2

let empty = (0, empty)

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
  include Iter.Set.Make (struct
    type t = clause

    let compare (id1, _) (id2, _) = Int.compare id1 id2
  end)

  let empty () = empty

  let fold f init s = fold f s init

  let show s = fold (fun c s -> Printf.sprintf "%s:%s\n" s (show c)) "" s
end
