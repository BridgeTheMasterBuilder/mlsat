type t =
  | Decision of {literal: Literal.t; level: int}
  | Implication of {literal: Literal.t; level: int; implicant: Clause.t}

type assignment = t

let literal = function
  | Decision {literal; _} | Implication {literal; _} ->
      literal

module Map = struct
  module M = CCPersistentHashtbl.Make (Variable)
  include M

  type t = assignment M.t

  let add l ass a = add a l ass

  let assignments a =
    let open Iter in
    to_iter a |> map (fun (_, ass) -> literal ass) |> to_list

  let find l a = find a l

  let find_opt = get

  let mem l a = mem a l

  let size = length

  let value l a =
    try
      let l' = find (Literal.var l) a |> literal in
      Tribool.of_bool (Literal.same_polarity l l')
    with Not_found -> Tribool.unknown

  module Cached = struct
    type uncached = t

    include Array

    (* type key = Variable.t *)

    type t = Literal.t array

    let add l ass m =
      unsafe_set m (Variable.to_int l) (literal ass) ;
      m

    let clear m =
      fill m 0 (length m) Literal.invalid ;
      m

    let make n = make (n + 1) Literal.invalid

    let mem l m =
      not (Literal.equal (unsafe_get m (Variable.to_int l)) Literal.invalid)

    let refresh m a =
      clear m |> ignore ;
      M.iter a (fun l ass -> unsafe_set m (Variable.to_int l) (literal ass)) ;
      m

    let[@inline] value l m =
      Asm.Assignment.value (Literal.to_int l) (Literal.Array.to_int_array m)
      |> Tribool.of_int
  end
end

let compare ass1 ass2 =
  match (ass1, ass2) with
  | Decision {level= d1; _}, Implication {level= d2; _} when d1 = d2 ->
      1
  | Implication {level= d1; _}, Decision {level= d2; _} when d1 = d2 ->
      -1
  | Decision {level= d1; _}, Decision {level= d2; _}
  | Decision {level= d1; _}, Implication {level= d2; _}
  | Implication {level= d1; _}, Decision {level= d2; _}
  | Implication {level= d1; _}, Implication {level= d2; _} ->
      -Int.compare d1 d2

let level = function Decision {level; _} | Implication {level; _} -> level

let show = function
  | Decision {literal; level} ->
      Printf.sprintf "\t%s because of decision on level %d\n"
        (Literal.show literal) level
  | Implication {literal; level; implicant} ->
      Printf.sprintf "\t%s because of clause %s - implied at level %d\n"
        (Literal.show literal) (Clause.show implicant) level

let was_decided_on_level d = function
  | Decision {level= d'; _} ->
      d = d'
  | _ ->
      false
