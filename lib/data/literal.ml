module Map = Map.Make (Int)
module Set = Iter.Set.Make (Int)

module List = struct
  let of_int_list = Fun.id
end

include Int

let hash = Fun.id
let is_negated l = l < 0
let of_int i = if i = 0 then failwith "Invalid literal" else i
let show = string_of_int
let signum = sign
let to_int = Fun.id
let var = Variable.of_int_unchecked
