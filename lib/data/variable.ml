module Map = Map.Make (Int)
module Set = Iter.Set.Make (Int)
include Int

let hash = Fun.id
let of_int i = if i = 0 then failwith "Invalid variable" else abs i
let of_int_unchecked = abs
let show = string_of_int
let to_int = Fun.id
