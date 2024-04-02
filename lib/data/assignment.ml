type t =
  | Decision of { literal : Literal.t; level : int }
  | Implication of { literal : Literal.t; level : int; implicant : Clause.t }

type assignment = t

let literal = function
  | Decision { literal; _ } | Implication { literal; _ } -> literal

(* module Map = struct *)
(*   include Variable.Map *)

(*   type t = assignment Variable.Map.t *)

(*   let assignments m = *)
(*     to_iter m |> Iter.map (fun (_, ass) -> literal ass) |> Iter.to_list *)

(*   let value l a = *)
(*     find_opt (Literal.var l) a *)
(*     |> Option.map (fun ass -> Literal.signum (literal ass) = Literal.signum l) *)
(*     |> Tribool.of_bool_opt *)
(* end *)
module Map = struct
  module M = CCPersistentHashtbl.Make (Variable)
  include M

  type t = assignment M.t

  let add l ass m = add m l ass

  let assignments m =
    to_iter m |> Iter.map (fun (_, ass) -> literal ass) |> Iter.to_list

  let empty = empty ()
  let find = Fun.flip find
  let find_opt = get
  let mem = Fun.flip mem

  let value l a =
    get (Literal.var l) a
    |> Option.map (fun ass -> Literal.signum (literal ass) = Literal.signum l)
    |> Tribool.of_bool_opt
end

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
