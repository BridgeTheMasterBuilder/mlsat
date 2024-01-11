(* open Common *)
include Literal.Set

type clause = t

let of_int_list ls = Iter.(of_list ls |> map Literal.of_int |> of_iter)
let size = cardinal

let show c =
  "(" ^ fold (fun l s -> Printf.sprintf "%s%s " s (Literal.show l)) c "" ^ ")"

let to_set = Fun.id

module Map = struct
  type t = clause CCVector.vector
  type key = int

  let add c m =
    CCVector.push m c;
    m

  let empty = CCVector.create ()
  let find c m = CCVector.get m c
  let is_empty = CCVector.is_empty

  let show map_str =
    CCVector.foldi
      (fun c s ls -> Printf.sprintf "%s%d:%s\n" s c (show ls))
      "" map_str

  let size = CCVector.length

  let to_iter m =
    CCVector.to_list m |> Iter.of_list |> Iter.mapi (fun i x -> (i, x))
end
