open Common
include IntMap

type t = Clause.t IntMap.t
type key = int

let sz = ref 0

let add n c =
  sz := !sz + 1;
  add n c

let show map_str =
  fold
    (fun c ls s -> Printf.sprintf "%s%d:%s\n" s c (Clause.show ls))
    map_str ""

let size _ = !sz
