module Frequency = struct
  type t = float

  let compare x y = -Float.(compare x y)
end

module Map = struct
  include Psq.Make (Literal) (Frequency)

  type nonrec t = { m : t; increase : float }
  type v = float

  let decay_factor = 0.99 (* TODO *)

  (* let decay = map (fun _ f -> f *. decay_factor) *)
  let decay { m; increase } =
    let increase' = increase *. (1.0 /. decay_factor) in
    { m; increase = increase' }

  let decr_iter iterator ({ m; _ } as t) =
    let open Iter in
    let m' =
      fold
        (fun m' l ->
          update l
            (function Some count -> Some (count -. 1.0) | None -> None)
            m')
        m iterator
    in
    { t with m = m' }

  let empty () = { m = empty; increase = 1.0 }

  let incr_iter iterator ({ m; increase } as t) =
    let open Iter in
    let m' =
      fold
        (fun m' l ->
          update l
            (function
              | Some count -> Some (count +. increase) | None -> Some increase)
            m')
        m iterator
    in
    { t with m = m' }

  let is_empty { m; _ } = is_empty m
  let mem l { m; _ } = mem l m

  (* let merge = ( ++ ) *)
  let pop { m; _ } = pop_exn m |> fst |> fst
  let remove_literal l ({ m; _ } as t) = { t with m = remove l m }

  let show { m; _ } =
    to_priority_list m
    |> List.fold_left
         (fun s (l, c) -> Printf.sprintf "%s%s:%f\n" s (Literal.show l) c)
         ""

  let to_iter { m; _ } = to_seq m |> Iter.of_seq
end
