open Common

type t = IntSet.t
type occurrence = t

module Map = struct
  (* include Literal.Map *)
  include Array

  (* type t = occurrence Literal.Map.t *)
  type t = occurrence option Array.t
  type key = Literal.t

  let foo l m =
    let open Literal in
    let l' = (var l |> to_int) - 1 + if is_negated l then length m / 2 else 0 in
    l'

  let unfoo l m =
    let open Literal in
    let l' =
      l - (if l >= length m / 2 then length m / 2 else 0) + 1 |> of_int
    in
    l'

  (* val add : key -> occurrence -> t -> t *)
  let add l x m =
    set m (foo l m) (Some x);
    m

  let create size = make (size * 2) None

  (* val empty : t *)
  (* val find : key -> t -> occurrence *)
  let find l m = get m (foo l m) |> Option.get_exn_or "FIND"

  (* val find_opt : key -> t -> occurrence option *)
  let find_opt l m = get m (foo l m)

  (* val mem : key -> t -> bool *)
  let mem l m = find_opt l m |> Option.is_some

  (* val show : t -> string *)
  (* val to_iter : t -> (key * occurrence) Iter.iter *)
  let to_iter m =
    let open Iter in
    of_array m
    |> filter_mapi (fun k v -> Option.map (fun v' -> (unfoo k m, v')) v)

  (* val update : key -> (occurrence option -> occurrence option) -> t -> t *)
  let update l f m =
    set m (foo l m) (f (get m (foo l m)));
    m
  (* let show o = *)
  (*   fold *)
  (*     (fun l cs s -> *)
  (*       Printf.sprintf "%s%s:%s\n" s (Literal.show l) *)
  (*         (IntSet.fold (fun l acc -> Printf.sprintf "%s%d " acc l) cs "")) *)
  (*     o "" *)
end
