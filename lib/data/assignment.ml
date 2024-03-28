type t =
  | Decision of { literal : Literal.t; level : int }
  | Implication of { literal : Literal.t; level : int; implicant : Clause.t }

type assignment = t

let literal = function
  | Decision { literal; _ } | Implication { literal; _ } -> literal

module Map = struct
  include Literal.Map

  type t = assignment Literal.Map.t

  let add l = add (Literal.var l)

  let assignments m =
    to_iter m |> Iter.map (fun (_, ass) -> literal ass) |> Iter.to_list

  let find_opt l = find_opt (Literal.var l)
  let mem l = mem (Literal.var l)
end

let value l a =
  Map.find_opt l a
  |> Option.map (fun ass -> Literal.signum (literal ass) = Literal.signum l)

(* TODO swap arguments? *)
let is_false l a = value l a |> Option.map_or ~default:false not
let is_true l a = value l a |> Option.map_or ~default:false Fun.id
let is_undefined l a = value l a |> Option.is_none
let level = function Decision { level; _ } | Implication { level; _ } -> level

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
