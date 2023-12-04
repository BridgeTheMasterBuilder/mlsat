include Literal.Set

let of_list ls = Iter.(of_list ls |> map Literal.of_int |> of_iter)
let size = cardinal

let show c =
  "(" ^ fold (fun l s -> Printf.sprintf "%s%s " s (Literal.show l)) c "" ^ ")"

let to_set = Fun.id

(* include List *)

(* type t = Literal.t list *)

(* let add = add_nodup ~eq:Literal.equal *)
(* let choose = hd *)
(* let fold f c s = fold_left (Fun.flip f) s c *)
(* let of_list = sort_uniq ~cmp:Literal.compare *)
(* let remove l = remove ~eq:Literal.equal ~key:l *)
(* let size = length *)

(* let show c = *)
(*   "(" *)
(*   ^ fold_left (fun s l -> Printf.sprintf "%s%s " s (Literal.show l)) "" c *)
(*   ^ ")" *)

(* let to_list = Fun.id *)
(* let to_set = Literal.Set.of_list *)
