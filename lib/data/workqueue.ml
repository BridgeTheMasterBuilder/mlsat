module type S = sig
  type elt
  type t
  type elt_set

  val create : unit -> t
  val empty : t
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val is_empty : t -> bool
  val of_iter : elt Iter.iter -> t
  val pop : t -> (elt * t) option
  val pop_exn : t -> elt * t
  val push : elt -> t -> t
  val push_iter : elt Iter.iter -> t -> t
  val singleton : elt -> t
end

module Make (Ord : Set.OrderedType) : S with type elt = Ord.t = struct
  module M = Set.Make (Ord)

  type elt = M.elt
  type elt_set = M.t
  type t = { queue : elt CCDeque.t; seen : elt_set }

  let create () = { queue = CCDeque.create (); seen = M.empty }
  let empty = { queue = CCDeque.create (); seen = M.empty }
  let fold f init { queue; _ } = CCDeque.fold f init queue
  let is_empty { queue; _ } = CCDeque.is_empty queue

  let of_iter iterator =
    { queue = CCDeque.of_iter iterator; seen = M.of_iter iterator }

  let pop ({ queue; _ } as w) =
    CCDeque.take_front_opt queue |> Option.map (fun x -> (x, w))

  let pop_exn ({ queue; _ } as w) = (CCDeque.take_front queue, w)

  let push x { queue; seen } =
    {
      queue =
        (if M.mem x seen then queue
         else (
           CCDeque.push_back queue x;
           queue));
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

  let singleton x =
    let queue = CCDeque.create () in
    CCDeque.push_back queue x;
    { queue; seen = M.singleton x }
end
