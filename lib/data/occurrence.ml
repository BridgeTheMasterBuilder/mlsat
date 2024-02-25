open Common

type t = IntSet.t
type occurrence = t

module Map = struct
  include Literal.Map

  type t = occurrence Literal.Map.t
  type key = Literal.t

  let add l n =
    update l (function
      | Some s -> Some (IntSet.add n s)
      | None -> Some (IntSet.singleton n))

  let show o =
    fold
      (fun l cs s ->
        Printf.sprintf "%s%s:%s\n" s (Literal.show l)
          (IntSet.fold (fun c acc -> Printf.sprintf "%s%d " acc c) cs ""))
      o ""
end
