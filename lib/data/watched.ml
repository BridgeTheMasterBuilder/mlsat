module L = Literal

type watched_clause =
  {clause: Clause.t; size: int; index: int; watched_literals: L.t * L.t}

module ClauseSet = struct
  include Iter.Set.Make (struct
    type t = watched_clause

    let compare {clause= c1; _} {clause= c2; _} = Clause.compare c1 c2
  end)

  let fold f init s = fold f s init
end

module Literal = struct
  module Map = struct
    module M = CCPersistentHashtbl.Make (L)
    include M

    type t = ClauseSet.t M.t

    type key = L.t

    let add l n m =
      update m l (function
        | Some s ->
            Some (ClauseSet.add n s)
        | None ->
            Some (ClauseSet.singleton n) )

    let fold f init m = fold (fun s l cs -> f l cs s) m init

    let make = create

    let find_opt = get

    let is_empty m = length m = 0

    let remove l n m =
      update m l (function
        | Some s ->
            Some (ClauseSet.remove n s)
        | None ->
            None )

    let show m =
      fold
        (fun l cs s ->
          Printf.sprintf "%s%s:%s\n" s (L.show l)
            (ClauseSet.fold
               (fun {clause; _} acc ->
                 Printf.sprintf "%s( %s) " acc (Clause.show clause) )
               "" cs ) )
        m ""
  end
end

module Clause = struct
  type t = watched_clause

  module Map = struct
    module M = CCPersistentHashtbl.Make (Clause)
    include M

    type t = watched_clause M.t

    type key = Clause.t

    let add c wc m = add m c wc

    let make = create

    let find_opt = get

    let remove l m = remove m l
  end

  module Set = ClauseSet

  type update_result =
    | WatchedLiteralChange of watched_clause * Literal.Map.t
    | Unit of L.t * Clause.t
    | Falsified of Clause.t
    | NoChange

  let to_clause {clause; _} = clause

  let unwatch ({watched_literals= w1, w2; _} as watched_clause) watched_literals
      =
    Literal.Map.remove w1 watched_clause watched_literals
    |> Literal.Map.remove w2 watched_clause

  let update l c
      ({clause; size; index; watched_literals= w1, w2; _} as watched_clause)
      watched_literals =
    let other_watched_literal = if L.equal l w1 then w2 else w1 in
    let other_watched_literal_truth_value =
      Assignment.Map.value other_watched_literal c
    in
    if Tribool.is_true other_watched_literal_truth_value then NoChange
    else
      let result =
        let open Iter in
        0 -- (size - 1)
        |> find_map (fun i ->
               let index' =
                 let index' = index + i in
                 if index' >= size then index' - size else index'
               in
               let clause = Clause.to_array clause in
               let l' = Array.unsafe_get clause index' in
               if
                 Tribool.is_false (Assignment.Map.value l' c)
                 || L.equal l' other_watched_literal
               then None
               else Some (index', l') )
      in
      match result with
      | None ->
          if Tribool.is_false other_watched_literal_truth_value then
            Falsified clause
          else Unit (other_watched_literal, clause)
      | Some (index', new_watched_literal) ->
          let watched_clause' =
            { watched_clause with
              index= index'
            ; watched_literals= (other_watched_literal, new_watched_literal) }
          in
          let watched_literals' =
            Literal.Map.remove l watched_clause watched_literals
            |> Literal.Map.remove other_watched_literal watched_clause
            |> Literal.Map.add new_watched_literal watched_clause'
            |> Literal.Map.add other_watched_literal watched_clause'
          in
          WatchedLiteralChange (watched_clause', watched_literals')

  let watch c clause watched_literals =
    let size = Clause.size clause in
    Clause.to_iter clause
    |> Iter.map (fun l -> (l, Assignment.Map.value l c))
    |> Iter.filter (fun (_, v) -> Tribool.is_nonfalse v)
    |> Iter.take 2 |> Iter.to_list
    |> function
    | [(w1, _); (w2, _)] ->
        let watched_clause =
          {clause; size; index= 2 mod size; watched_literals= (w1, w2)}
        in
        let watched_literals' =
          Literal.Map.add w1 watched_clause watched_literals
          |> Literal.Map.add w2 watched_clause
        in
        WatchedLiteralChange (watched_clause, watched_literals')
    | [(w, v)] ->
        if Tribool.is_unknown v then Unit (w, clause) else NoChange
    | _ ->
        Falsified clause
end
