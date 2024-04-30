module Frequency = struct
  type t = float

  let compare x y = -Float.(compare x y)
end

module Map = struct
  include Psq.Make (Literal) (Frequency)

  type nonrec t = {m: t; increase: float; decay_factor: float}

  type v = float

  let decay ({m; increase; decay_factor} as t) =
    let increase' = increase *. (1.0 /. decay_factor) in
    match Float.classify increase' with
    | FP_infinite ->
        {t with increase= 1.0}
    | _ ->
        {t with increase= increase'}

  let pop_exn m = pop m |> Option.get_exn_or "POP"

  let flush_assigned c ({m; _} as t) =
    let rec aux m' =
      match min m' with
      | Some (l, _) ->
          if Assignment.Map.Cached.mem (Literal.var l) c then
            aux (pop_exn m' |> snd)
          else {t with m= m'}
      | None ->
          {t with m= m'}
    in
    aux m

  let incr_iter iterator ({m; increase; _} as t) =
    let open Iter in
    let m' =
      fold
        (fun m' l ->
          update l
            (function
              | Some count -> Some (count +. increase) | None -> Some increase
              )
            m' )
        m iterator
    in
    {t with m= m'}

  let is_empty {m; _} = is_empty m

  let make decay_factor = {m= empty; increase= 1.0; decay_factor}

  let mem l {m; _} = mem l m

  let merge {m; _} ({m= m'; _} as t) = {t with m= m ++ m'}

  let min_exn {m; _} = min m |> Option.get_exn_or "MIN" |> fst

  let pop {m; _} = pop_exn m |> fst |> fst

  let remove_literal l ({m; _} as t) = {t with m= remove l m}

  let show {m; _} =
    to_priority_list m
    |> List.fold_left
         (fun s (l, c) -> Printf.sprintf "%s%s:%f\n" s (Literal.show l) c)
         ""

  let to_iter {m; _} = to_seq m |> Iter.of_seq
end
