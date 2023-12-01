open Common
include IntMap

type t = Clause.t IntMap.t
type key = Literal.t

let show map_str =
  fold
    (fun c ls s -> Printf.sprintf "%s%d:%s\n" s c (Clause.show ls))
    map_str ""

let size m = max_binding_opt m |> Option.map_or ~default:0 fst
