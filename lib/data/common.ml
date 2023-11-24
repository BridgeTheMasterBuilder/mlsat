(* module IntSet = struct *)
(*   include Iter.Set.Make (Int) *)

(*   let show set = *)
(*     fold *)
(*       (fun elt s -> *)
(*         (if String.equal s "" then "{" else s ^ ", ") ^ string_of_int elt) *)
(*       set "" *)
(*     ^ "}" *)
(* end *)

module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)
