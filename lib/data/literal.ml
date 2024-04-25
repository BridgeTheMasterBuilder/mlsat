module Map = Map.Make (Int)
module Set = Iter.Set.Make (Int)

module List = struct
  let of_int_list = Fun.id
end

include Int

let compare = ( - )

let hash = Fun.id

let of_int_unchecked = Fun.id

let of_var = Variable.to_int

let show = string_of_int

let signum = sign

let same_polarity l1 l2 = signum l1 = signum l2

let var = Variable.of_int_unchecked
