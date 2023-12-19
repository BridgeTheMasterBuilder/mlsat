module Frequency = struct
  type t = float

  let compare x y = -Float.(compare x y)
end

include Psq.Make (Literal) (Frequency)

let decay_factor = 0.99

let add_many =
  Clause.fold (fun l m' ->
      update l
        (function
          | Some count ->
              (* Printf.printf *)
              (*   "Updating frequency count for literal %s from %f to %f\n" *)
              (*   (Literal.show l) count (count +. 1.0); *)
              Some (count +. 1.0)
          | None -> Some 1.0)
        m')

let decay m =
  fold (fun l f m' -> adjust l (Fun.const (f *. decay_factor)) m') m m

let pop m = pop m |> Option.map (fun ((l, _), _) -> l)
let remove_literal = remove

let remove_clause l =
  update l (function
    | Some count ->
        let count' = count -. 1.0 in
        if count' >. 0.0 then Some count' else None
    | None -> None)

let remove_clauses =
  Clause.fold (fun l m' ->
      update l
        (function
          | Some count ->
              let count' = count -. 1.0 in
              if count' >. 0.0 then Some count' else None
          | None -> None)
        m')

let show o =
  to_priority_list o
  |> List.fold_left
       (fun s (l, c) -> Printf.sprintf "%s%s:%f\n" s (Literal.show l) c)
       ""
