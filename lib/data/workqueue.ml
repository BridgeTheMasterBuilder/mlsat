module type S = sig
  type elt
  type t
  type elt_set

  val is_empty : t -> bool
  val of_iter : elt Iter.iter -> t
  val pop : t -> (elt * t) option
  val push : elt -> t -> t
  val push_iter : elt Iter.iter -> t -> t
end

module Make (Ord : Set.OrderedType) : S with type elt = Ord.t = struct
  module M = Set.Make (Ord)

  type elt = M.elt
  type elt_set = M.t
  type t = { queue : elt CCDeque.t; seen : elt_set }

  let is_empty { queue; _ } = CCDeque.is_empty queue

  let of_iter iterator =
    { queue = CCDeque.of_iter iterator; seen = M.of_iter iterator }

  let pop ({ queue; _ } as w) =
    CCDeque.take_front_opt queue |> Option.map (fun x -> (x, w))

  let push x { queue; seen } =
    {
      queue =
        (CCDeque.push_back queue x;
         queue);
      seen = M.add x seen;
    }

  let push_iter iterator { queue; seen } =
    let open Iter in
    let unseen = filter (fun x -> not M.(mem x seen)) iterator in
    {
      queue =
        (CCDeque.add_iter_back queue unseen;
         queue);
      seen = M.(union seen (of_iter unseen));
    }
end
