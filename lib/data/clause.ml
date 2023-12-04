include Literal.Set

let of_int_list ls = Iter.(of_list ls |> map Literal.of_int |> of_iter)
let size = cardinal

let show c =
  "(" ^ fold (fun l s -> Printf.sprintf "%s%s " s (Literal.show l)) c "" ^ ")"

let to_set = Fun.id
