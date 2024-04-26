module Frequency = struct
  type t = int

  let compare x y = -Int.(compare x y)
end

module Map = struct
  include Psq.Make (Literal) (Frequency)

  type v = int

  type nonrec t = {m: t; increase: v}

  let decay {m; increase} =
    let increase' = increase * 2 in
    if increase' < increase then
      let m' =
        fold
          (fun l _ m' ->
            update l
              (function Some count -> Some (count / increase) | None -> None)
              m' )
          m m
      in
      {m= m'; increase= 1}
    else {m; increase= increase'}

  let empty () = {m= empty; increase= 1}

  let pop_exn m = pop m |> Option.get_exn_or "POP"

  let flush_assigned a ({m; _} as t) =
    let rec aux m' =
      match min m' with
      | Some (l, _) ->
          if Assignment.Map.mem (Literal.var l) a then aux (pop_exn m' |> snd)
          else {t with m= m'}
      | None ->
          {t with m= m'}
    in
    aux m

  let incr_iter iterator ({m; increase} as t) =
    let open Iter in
    let m' =
      fold
        (fun m' l ->
          update l
            (function
              | Some count -> Some (count + increase) | None -> Some increase )
            m' )
        m iterator
    in
    {t with m= m'}

  let is_empty {m; _} = is_empty m

  let mem l {m; _} = mem l m

  let merge {m; _} {m= m'; increase} = {m= m ++ m'; increase}

  let min_exn {m; _} = min m |> Option.get_exn_or "MIN" |> fst

  let pop {m; _} = pop_exn m |> fst |> fst

  let remove_literal l ({m; _} as t) = {t with m= remove l m}

  let show {m; _} =
    to_priority_list m
    |> List.fold_left
         (fun s (l, c) -> Printf.sprintf "%s%s:%d\n" s (Literal.show l) c)
         ""

  let to_iter {m; _} = to_seq m |> Iter.of_seq
end
