type t = int

let unknown = 0

let is_false x = x < 0

let is_known x = x <> 0

let is_nonfalse x = x >= 0

let is_nontrue x = x < 1

let is_true x = x > 0

let is_unknown x = x = 0

let of_bool = function true -> 1 | false -> -1

let of_bool_opt = function Some true -> 1 | Some false -> -1 | None -> 0

let of_int = Fun.id
