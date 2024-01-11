open Common

type t = IntSet.t
type occurrence = t

module Map = struct
  (* include Literal.Map *)
  include Array

  (* type t = occurrence Literal.Map.t *)
  type t = occurrence option Array.t
  type key = Literal.t

  let foo l =
    let open Literal in
    let l' = to_int l in
    (* Printf.printf "foo: %s -> %s\n" (show l) (string_of_int l'); *)
    l'

  let unfoo l =
    let open Literal in
    let l' = of_int_raw l in
    (* Printf.printf "unfoo: %s -> %s\n" (string_of_int l) (show l'); *)
    l'

  (* val add : key -> occurrence -> t -> t *)
  let add l x m =
    set m (foo l) (Some x);
    m

  let create size = make ((size * 2) + 1) None

  (* val empty : t *)
  (* val find : key -> t -> occurrence *)
  let find l m = get m (foo l) |> Option.get_exn_or "FIND"

  (* val find_opt : key -> t -> occurrence option *)
  let find_opt l m = get m (foo l)

  (* val mem : key -> t -> bool *)
  let mem l m = find_opt l m |> Option.is_some

  (* val show : t -> string *)
  (* val to_iter : t -> (key * occurrence) Iter.iter *)
  let to_iter m =
    let open Iter in
    of_array m
    |> filter_mapi (fun k v -> Option.map (fun v' -> (unfoo k, v')) v)

  (* val update : key -> (occurrence option -> occurrence option) -> t -> t *)
  let update l f m =
    set m (foo l) (f (get m (foo l)));
    m
  (* let show o = *)
  (*   fold *)
  (*     (fun l cs s -> *)
  (*       Printf.sprintf "%s%s:%s\n" s (Literal.show l) *)
  (*         (IntSet.fold (fun l acc -> Printf.sprintf "%s%d " acc l) cs "")) *)
  (*     o "" *)
end
