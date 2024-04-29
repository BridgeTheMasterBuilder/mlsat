(* type t = True | False | Unknown *)

(* let is_false = function False -> true | _ -> false *)
(* let is_known = function Unknown -> false | _ -> true *)
(* let is_nonfalse = function False -> false | _ -> true *)
(* let is_nontrue = function True -> false | _ -> true *)
(* let is_true = function True -> true | _ -> false *)
(* let is_unknown = function Unknown -> true | _ -> false *)

(* let of_bool_opt = function *)
(*   | Some true -> True *)
(*   | Some false -> False *)
(*   | None -> Unknown *)

(* let to_bool_opt = function *)
(*   | True -> Some true *)
(*   | False -> Some false *)
(*   | Unknown -> None *)

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

(* let to_bool_opt = function *)
(*   | 1 -> *)
(*       Some true *)
(*   | -1 -> *)
(*       Some false *)
(*   | 0 -> *)
(*       None *)
(*   | _ -> *)
(*       failwith "Impossible" *)
