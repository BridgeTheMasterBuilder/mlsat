type t = True | False | Unknown

let of_bool_opt = function
  | Some true -> True
  | Some false -> False
  | None -> Unknown

let to_bool_opt = function
  | True -> Some true
  | False -> Some false
  | Unknown -> None

let is_true = function True -> true | _ -> false
let is_false = function False -> true | _ -> false
let is_unknown = function Unknown -> true | _ -> false
let is_nonfalse = function False -> false | _ -> true
let is_nontrue = function True -> false | _ -> true
let is_known = function Unknown -> false | _ -> true
