(* open Common *)
open Batteries
include Literal.Set

type clause = t

(* TODO lbd a c *)
let of_int_list ls = Iter.(of_list ls |> map Literal.of_int |> of_iter)
let size = cardinal
let show c = fold (fun l s -> Printf.sprintf "%s%s " s (Literal.show l)) c ""

module Map = struct
  type t = clause BatDynArray.t
  type key = int

  let add c m =
    BatDynArray.add m c;
    m

  let find = Fun.flip BatDynArray.unsafe_get
  let is_empty = BatDynArray.empty
  let make = BatDynArray.make

  let remove n m =
    BatDynArray.delete m n;
    m

  let show =
    BatDynArray.fold_lefti
      (fun s c ls -> Printf.sprintf "%s%d:%s\n" s c (show ls))
      ""

  let size = BatDynArray.length

  let to_iter m =
    BatDynArray.to_list m |> Iter.of_list |> Iter.mapi (fun i x -> (i, x))
end
