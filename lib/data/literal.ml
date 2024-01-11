module Map = Map.Make (Int)
module Set = Iter.Set.Make (Int)
include Int

let num_variables = ref 0
let is_negated l = l >= !num_variables

let neg l =
  (* if l >= !num_variables then l - !num_variables else l + !num_variables *)
  neg (l - !num_variables) + !num_variables

let of_int i =
  match sign i with
  | 1 -> i + !num_variables
  | -1 -> i + !num_variables
  | _zero -> failwith "Invalid literal"

let of_int_raw = Fun.id
let show l = string_of_int (l - !num_variables)
let signum l = sign (l - !num_variables)
let to_int = Fun.id

(* let var l = l + if l < !num_variables then !num_variables else 0 *)
let var l = abs (l - !num_variables)
