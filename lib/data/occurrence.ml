open Common

type t = IntSet.t
type occurrence = t

module Map = struct
  include Literal.Map

  type t = occurrence Literal.Map.t
  type key = Literal.t

  let show o =
    fold
      (fun l cs s ->
        Printf.sprintf "%s%s:%s\n" s (Literal.show l)
          (IntSet.fold (fun l acc -> Printf.sprintf "%s%d " acc l) cs ""))
      o ""
end
