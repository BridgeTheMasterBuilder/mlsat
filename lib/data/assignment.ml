type t =
  | Decision of { literal : Literal.t; level : int }
  | Implication of {
      literal : Literal.t;
      level : int;
      implicant : Literal.t array;
    }

type assignment = t

let literal = function
  | Decision { literal; _ } | Implication { literal; _ } -> literal

module Map = struct
  module M = CCPersistentHashtbl.Make (Variable)
  include M

  type t = assignment M.t

  let add l ass m = add m l ass

  let assignments m =
    to_iter m |> Iter.map (fun (_, ass) -> literal ass) |> Iter.to_list

  let find l m = find m l
  let find_opt = get
  let mem l m = mem m l

  let value l a =
    get (Literal.var l) a
    |> Option.map (fun ass -> Literal.signum (literal ass) = Literal.signum l)
    |> Tribool.of_bool_opt
end

let compare ass1 ass2 =
  match (ass1, ass2) with
  | Decision { level = d1; _ }, Implication { level = d2; _ } when d1 = d2 -> 1
  | Implication { level = d1; _ }, Decision { level = d2; _ } when d1 = d2 -> -1
  | Decision { level = d1; _ }, Decision { level = d2; _ }
  | Decision { level = d1; _ }, Implication { level = d2; _ }
  | Implication { level = d1; _ }, Decision { level = d2; _ }
  | Implication { level = d1; _ }, Implication { level = d2; _ } ->
      -Int.compare d1 d2

let level = function Decision { level; _ } | Implication { level; _ } -> level

module Clause = struct
  let show c =
    Array.fold (fun s l -> Printf.sprintf "%s%s " s (Literal.show l)) "" c
end

let show = function
  | Decision { literal; level } ->
      Printf.sprintf "\t%s because of decision on level %d\n"
        (Literal.show literal) level
  | Implication { literal; level; implicant } ->
      Printf.sprintf "\t%s because of clause %s - implied at level %d\n"
        (Literal.show literal) (Clause.show implicant) level

let was_decided_on_level d = function
  | Decision { level = d'; _ } -> d = d'
  | _ -> false
