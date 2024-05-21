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

module Make (E : Set.OrderedType) : S with type elt = E.t = struct
  module M = Iter.Set.Make (E)

  type elt = M.elt

  type elt_set = M.t

  type t = {queue: elt CCFQueue.t; seen: elt_set}

  let empty () =
    let seen = M.empty in
    {queue= CCFQueue.empty; seen}

  let fold f init {queue; _} = CCFQueue.fold f init queue

  let is_empty {queue; _} = CCFQueue.is_empty queue

  let of_iter iterator =
    {queue= CCFQueue.of_iter iterator; seen= M.of_iter iterator}

  let pop ({queue; _} as w) =
    let open Option.Infix in
    let+ elt, queue' = CCFQueue.take_front queue in
    (elt, {w with queue= queue'})

  let pop_exn ({queue; _} as w) =
    let elt, queue' = CCFQueue.take_front_exn queue in
    (elt, {w with queue= queue'})

  let push x {queue; seen} =
    { queue= (if M.mem x seen then queue else CCFQueue.snoc queue x)
    ; seen= M.add x seen }

  let push_iter iterator {queue; seen} =
    let open Iter in
    let unseen = filter (fun x -> not M.(mem x seen)) iterator in
    { queue= CCFQueue.add_iter_back queue unseen
    ; seen= M.(union seen (of_iter unseen)) }

  let singleton x = {queue= CCFQueue.singleton x; seen= M.singleton x}
end
