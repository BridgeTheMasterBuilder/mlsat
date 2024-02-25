module M = struct
  type t = {
    id : int; (* TODO hack? *)
    clause : Literal.t array;
    size : int;
    index : int;
    watchers : (Literal.t * Literal.t) option;
  }

  let compare { clause = c1; _ } { clause = c2; _ } =
    Array.compare (fun l1 l2 -> Literal.compare l1 l2) c1 c2
end

open M
module Set = Set.Make (M)

module Map = struct
  include Literal.Map

  type t = Set.t Literal.Map.t
  type key = Literal.t

  let add l n =
    update l (function
      | Some s -> Some (Set.add n s)
      | None -> Some (Set.singleton n))

  let show o =
    fold
      (fun l cs s ->
        Printf.sprintf "%s%s:%s\n" s (Literal.show l)
          (Set.fold (fun { id; _ } acc -> Printf.sprintf "%s%d " acc id) cs ""))
      o ""
end

let fold f x { clause; _ } = Array.fold f x clause

let of_clause c id =
  (* TODO assert clause is not empty? *)
  let open Iter in
  let clause = Clause.to_iter c |> to_array in
  let size = Array.length clause in
  let watchers = if size >= 2 then Some (clause.(0), clause.(1)) else None in
  { id; clause; size; index = 2; watchers }

let update a ({ size; _ } as c) =
  let open Iter in
  let c' =
    Array.iter (fun l -> print_string (Literal.show l ^ " ")) c.clause;
    0 -- (size - 1)
    |> fold_while
         (fun { clause; watchers; _ } i ->
           print_string (Literal.show clause.(i) ^ " ");
           (c, `Continue))
         c
  in
  print_newline ();
  c'

let watched_literals { watchers; _ } = watchers
