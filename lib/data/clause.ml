open Common
include Literal.Set

type clause = t

let of_int_list ls = Iter.(of_list ls |> map Literal.of_int |> of_iter)
let size = cardinal

let show c =
  "(" ^ fold (fun l s -> Printf.sprintf "%s%s " s (Literal.show l)) c "" ^ ")"

let to_set = Fun.id

module Map = struct
  include IntMap

  type t = clause IntMap.t
  type key = int

  let sz = ref 0

  let add n c =
    sz := !sz + 1;
    add n c

  let show map_str =
    fold (fun c ls s -> Printf.sprintf "%s%d:%s\n" s c (show ls)) map_str ""

  let size _ = !sz
end
