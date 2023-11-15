module Map = Map.Make (Int)

(* module Set = struct *)
(*   include Iter.Set.Make (Int) *)

(*   let to_int_list = to_list *)
(*   let to_int_set = Fun.id *)
(* end *)
module Set = Iter.Set.Make (Int)
include Int

let invalid = 0
let is_negated l = l < 0
let of_int i = if i = 0 then failwith "Invalid literal" else i
let var = abs
