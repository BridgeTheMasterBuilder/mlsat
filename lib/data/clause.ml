include Literal.Set

type t = { clause : Literal.Set.t; original : Literal.Set.t }

let choose { clause; _ } = choose clause
let empty = { clause = empty; original = empty }
let is_empty { clause; _ } = is_empty clause
let mem l { clause; _ } = mem l clause

let add l { clause; original } =
  { clause = add l clause; original = add l original }

let remove l { clause; _ } =
  let clause = remove l clause in
  { clause; original = clause }

let fold f { clause; _ } = fold f clause

let of_list ls =
  let clause = Iter.(of_list ls |> map Literal.of_int |> of_iter) in
  { clause; original = clause }

let original { original; _ } = { clause = original; original }
let size { clause; _ } = cardinal clause

let show { clause; _ } =
  "("
  ^ Literal.Set.fold
      (fun l s -> Printf.sprintf "%s%s " s (Literal.show l))
      clause ""
  ^ ")"

let to_iter { clause; _ } = to_iter clause
let to_list { clause; _ } = to_list clause
let to_set { clause; _ } = clause

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
