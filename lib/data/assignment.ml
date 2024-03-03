type t =
  | Decision of { literal : Literal.t; level : int }
  | Implication of { literal : Literal.t; level : int; implicant : Clause.t }

type assignment = t

let level = function Decision { level; _ } | Implication { level; _ } -> level

let literal = function
  | Decision { literal; _ } | Implication { literal; _ } -> literal

let show (a : assignment) =
  match a with
  | Decision { literal; level } ->
      Printf.sprintf "\t%s because of decision on level %d\n"
        (Literal.show literal) level
  | Implication { literal; level; implicant } ->
      Printf.sprintf "\t%s because of clause %s - implied at level %d\n"
        (Literal.show literal) (Clause.show implicant) level

let was_decided_on_level a d =
  match a with Decision { level = d'; _ } -> d = d' | _ -> false

module Map = struct
  include Literal.Map

  type t = assignment Literal.Map.t

  let add l = add (Literal.var l)

  let assignments m =
    to_iter m |> Iter.map (fun (_, ass) -> literal ass) |> Iter.to_list

  let find_opt l = find_opt (Literal.var l)
  let mem l = mem (Literal.var l)
end
