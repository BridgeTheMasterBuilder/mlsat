type t =
  | Decision of { literal : Literal.t; level : int }
  | Implication of { literal : Literal.t; level : int; implicant : Clause.t }

type assignment = t

(* TODO Why is it different? *)
module Map = struct
  (* include Literal.Map *)
  include Array

  (* type t = assignment Literal.Map.t *)
  type t = assignment option Array.t
  type key = Literal.t

  let foo l =
    let open Literal in
    let l' = (var l |> to_int) - 1 in
    (* let l'' = if is_negated l then l' + (length m / 2) else l' in *)
    Printf.printf "%s -> %s\n" (show l) (string_of_int l');
    (* l'' *)
    l'

  let add l x m =
    set m (foo l) (Some x);
    m

  let create size = make size None
  let find l m = get m (foo l) |> Option.get_exn_or "FIND"
  let find_opt l m = get m (foo l)
  let mem l m = find_opt l m |> Option.is_some
end

let level = function Decision { level; _ } | Implication { level; _ } -> level

let literal = function
  | Decision { literal; _ } | Implication { literal; _ } -> literal

let show (a : assignment) =
  match a with
  | Decision { literal; level } ->
      Printf.sprintf "\t%s because of decision on level %d\n"
        (Literal.show literal) level
  | Implication { literal; level; implicant } ->
      Printf.sprintf "\t%s because of clause (%s) - implied at level %d\n"
        (Literal.show literal) (Clause.show implicant) level

let was_decided_on_level a d =
  match a with Decision { level = d'; _ } -> d = d' | _ -> false
