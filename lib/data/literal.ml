module Map = Map.Make (Int)
module Set = Iter.Set.Make (Int)

module List = struct
  let of_int_list = Fun.id
end

include Int

let hash = Fun.id
let show = string_of_int
let signum = sign
let var = Variable.of_int_unchecked
