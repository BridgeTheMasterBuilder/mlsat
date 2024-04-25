module type S = sig
  type elt
  type t
  type elt_set

  val empty : unit -> t
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val is_empty : t -> bool
  val of_iter : elt Iter.iter -> t
  val pop : t -> (elt * t) option
  val pop_exn : t -> elt * t
  val push : elt -> t -> t
  val push_iter : elt Iter.iter -> t -> t
  val singleton : elt -> t
end

module Make (E : CCHashSet.ELEMENT) : S with type elt = E.t = struct
  module M = CCHashSet.Make (E)

  type elt = M.elt
  type elt_set = M.t
  type t = { queue : elt CCDeque.t; seen : elt_set }

  let empty () =
    (* TODO *)
    let seen = M.create 100 in
    { queue = CCDeque.create (); seen }

  let fold f init { queue; _ } = CCDeque.fold f init queue
  let is_empty { queue; _ } = CCDeque.is_empty queue

  let of_iter iterator =
    { queue = CCDeque.of_iter iterator; seen = M.of_iter iterator }

  let pop ({ queue; _ } as w) =
    let open Option.Infix in
    let+ queue' = CCDeque.take_front_opt queue in
    (queue', w)

  let pop_exn ({ queue; _ } as w) = (CCDeque.take_front queue, w)

  let push x { queue; seen } =
    let already_seen = M.mem seen x in
    {
      queue =
        (if already_seen then queue
         else (
           CCDeque.push_back queue x;
           queue));
      seen =
        (M.insert seen x;
         seen);
    }

  let push_iter iterator { queue; seen } =
    let open Iter in
    let unseen = filter (fun x -> not M.(mem seen x)) iterator in
    CCDeque.add_iter_back queue unseen;
    {
      queue;
      seen =
        M.(
          union_mut ~into:seen (of_iter unseen);
          seen);
    }

  let singleton x =
    let queue = CCDeque.create () in
    CCDeque.push_back queue x;
    { queue; seen = M.singleton x }
end
