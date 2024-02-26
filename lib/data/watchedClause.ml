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
  { id; clause; size; index = 2 mod size; watchers }

(* try *)
(*   let uc', f' = *)
(*     match Occurrence.Map.find_opt (Literal.neg l) occur with *)
(*     | Some cs -> *)
(*         IntSet.fold *)
(*           (fun c (uc', f') -> *)
(*             let ls = Clause.Map.find c clauses in *)
(*             let uc'', f'', unassigned, falsified, satisfied = *)
(*               Clause.fold *)
(*                 (fun l (uc'', f'', unassigned', falsified', satisfied') -> *)
(*                   match Assignment.Map.find_opt l a' with *)
(*                   | Some ass' -> *)
(*                       let l' = Assignment.literal ass' in *)
(*                       let falso = Literal.signum l <> Literal.signum l' in *)
(*                       ( uc'', *)
(*                         f'', *)
(*                         unassigned', *)
(*                         falsified' && falso, *)
(*                         satisfied' || not falso ) *)
(*                   | None -> *)
(*                       (uc'', f'', Clause.add l unassigned', false, satisfied')) *)
(*                 ls *)
(*                 (uc', f', Clause.empty, true, false) *)
(*             in *)
(*             if satisfied then (uc'', f'') *)
(*             else if falsified then raise_notrace (Conflict (ls, f)) *)
(*             else if Clause.size unassigned = 1 then ((c, ls) :: uc'', f'') *)
(*             else (uc'', f'')) *)
(*           cs (uc, f) *)
(*     | None -> (uc, f) *)
(*   in *)

(* TODO update_result type to know if the watchers need updating, unit propagation is ready, or the clause has been falsified *)
let update l a ({ clause; size; index; watchers; _ } as c) =
  let update_watchers w w' =
    let w1, w2 = Option.get_exn_or "UPDATE_WATCHERS" watchers in
    if Literal.equal w' w1 || Literal.equal w' w2 then None
    else Some (if Literal.equal w w1 then (w', w2) else (w1, w'))
  in
  let open Iter in
  let w1, w2 = Option.get_exn_or "UPDATE" watchers in
  Printf.printf "%d (watching %s and %s - index %d): " c.id (Literal.show w1)
    (Literal.show w2) index;
  let c', index' =
    Array.iter (fun l -> print_string (Literal.show l ^ " ")) c.clause;
    print_string "= ";
    0 -- (size - 1)
    (* TODO falsified clause *)
    |> fold_while
         (fun (c, index') _ ->
           let index'' = (index' + 1) mod size in
           let l' = clause.(index') in
           match Assignment.Map.find_opt l' a with
           | Some x ->
               Printf.printf "%s(%s) " (Literal.show l')
                 (Literal.show (Assignment.literal x));
               ((c, index''), `Continue)
           | None ->
               Printf.printf "%s(_) " (Literal.show l');
               (* if Literal.equal l l' then print_endline "WRONG"; *)
               (({ c with watchers = update_watchers l l' }, index''), `Stop))
         (*   (\* print_string (Literal.show clause.(i) ^ " "); *\) *)
         (*   Printf.printf "%s(%s) " (Literal.show l) *)
         (*   (Assignment.Map.find_opt l a *)
         (*   |> Option.map_or ~default:"_" (fun x -> *)
         (*          Literal.show (Assignment.literal x))); *)
         (* (c, `Continue)) *)
         (c, index)
  in
  print_newline ();
  (match c'.watchers with
  | Some (w1, w2) ->
      Printf.printf "%d (watching %s and %s - index %d)\n\n" c'.id
        (Literal.show w1) (Literal.show w2) index'
  | None -> Printf.printf "Clause %d is ready for unit propagation\n" c'.id);
  { c' with index = index' }

let watched_literals { watchers; _ } = watchers
(* TODO this is more or less unneeded, replaced by watcher update *)
