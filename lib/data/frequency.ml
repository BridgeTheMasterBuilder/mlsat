module Frequency = struct
  type t = float

  let compare x y = -Float.(compare x y)
end

module Map = struct
  include Psq.Make (Literal) (Frequency)

  type v = float

  let decay_factor = 0.5 (* TODO *)

  let decay = map (fun _ f -> f *. decay_factor)

  let decr_iter iterator m =
    let open Iter in
    fold
      (fun m' l ->
        update l
          (function Some count -> Some (count -. 1.0) | None -> None)
          m' )
      m iterator

  let empty () = empty

  let flush_assigned a m =
    let rec aux m' =
      match min m' with
      | Some (l, _) ->
          if Assignment.Map.mem (Literal.var l) a then aux (pop_exn m' |> snd)
          else m'
      | None ->
          m'
    in
    aux m

  let incr_iter iterator m =
    let open Iter in
    fold
      (fun m' l ->
        update l
          (function Some count -> Some (count +. 1.0) | None -> Some 1.0)
          m' )
      m iterator

  let merge = ( ++ )

  let min_exn m = min m |> Option.get_exn_or "MIN" |> fst

  let pop m = pop_exn m |> fst |> fst

  let remove_literal = remove

  let show m =
    to_priority_list m
    |> List.fold_left
         (fun s (l, c) -> Printf.sprintf "%s%s:%f\n" s (Literal.show l) c)
         ""

  let to_iter m = to_seq m |> Iter.of_seq
end
