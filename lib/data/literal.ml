module Map = Map.Make (Int)
module Set = Iter.Set.Make (Int)

module List = struct
  (* let of_int_list = Fun.id *)
  let of_int_list = List.map (fun l -> l lsl 1)
end

include Int

let invalid = 0

let encode x = x lsl 1

let decode l = l asr 1

let compare = ( - )

let hash = Fun.id

(* let of_int_unchecked = Fun.id *)
let of_int_unchecked = encode

(* let of_var = Variable.to_int *)
let of_var v = Variable.to_int v |> encode

let show l = string_of_int (decode l)

let signum = sign

let same_polarity l1 l2 = signum l1 = signum l2

(* let to_int = Fun.id *)
let to_int = decode

(* let var = Variable.of_int_unchecked *)
let var l = decode l |> Variable.of_int_unchecked
