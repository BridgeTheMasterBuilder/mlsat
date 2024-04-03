module Frequency = struct
  type t = float

  let compare x y = -Float.(compare x y)
end

module Map = struct
  include Psq.Make (Literal) (Frequency)

  let decay_factor = 0.99
  let decay = map (fun _ f -> f *. decay_factor)

  let decr_iter =
    let open Iter in
    Fun.flip
    @@ fold (fun m' l ->
           update l
             (function
               | Some count -> Some (count -. 1.0)
               (* | None -> failwith "Impossible") *)
               | None -> None)
             m')

  let incr_iter =
    let open Iter in
    Fun.flip
    @@ fold (fun m' l ->
           update l
             (function Some count -> Some (count +. 1.0) | None -> Some 1.0)
             m')

  let pop m = pop_exn m |> fst |> fst
  let remove_literal = remove

  let show m =
    to_priority_list m
    |> List.fold_left
         (fun s (l, c) -> Printf.sprintf "%s%s:%f\n" s (Literal.show l) c)
         ""

  let to_iter m = to_seq m |> Iter.of_seq
end
