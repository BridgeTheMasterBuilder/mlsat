module L = Literal

type watched_clause =
  { clause: Clause.t
  ; size: int
  ; mutable index: int
  ; mutable watched_literals: L.t * L.t }

module ClauseSet = struct
  include CCHashSet.Make (struct
    type t = watched_clause

    let equal {clause= c1; _} {clause= c2; _} = Clause.equal c1 c2

    let hash {clause; _} = Clause.hash clause
  end)

  let add c s = insert s c ; s

  let remove c s = remove s c ; s
end

module Literal = struct
  module Map = struct
    module M = CCHashtbl.Make (L)
    include M

    type t = ClauseSet.t M.t

    type key = L.t

    let add l n m =
      update m ~k:l ~f:(fun _ -> function
        | Some s ->
            Some (ClauseSet.add n s)
        | None ->
            Some (ClauseSet.singleton n) ) ;
      m

    let make n = create n

    let find_opt l m = find_opt m l

    let is_empty m = length m = 0

    let remove l n m =
      update m ~k:l ~f:(fun _ -> function
        | Some s -> Some (ClauseSet.remove n s) | None -> None ) ;
      m

    let show m =
      fold
        (fun l cs s ->
          Printf.sprintf "%s%s:%s\n" s (L.show l)
            (ClauseSet.fold
               (fun acc {clause; _} ->
                 Printf.sprintf "%s( %s) " acc (Clause.show clause) )
               "" cs ) )
        m ""
  end
end

module Clause = struct
  type t = watched_clause

  module Map = struct
    module M = CCHashtbl.Make (Clause)
    include M

    type t = watched_clause M.t

    type key = Clause.t

    let add c wc m = add m c wc ; m

    let make n = create n

    let find_opt l m = find_opt m l

    let remove l m = remove m l ; m
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
      Assignment.Map.Cached.value other_watched_literal c
    in
    if Tribool.is_true other_watched_literal_truth_value then NoChange
    else
      let exception Found of int * L.t in
      try
        for i = 0 to size - 1 do
          let index' =
            let index' = index + i in
            if index' >= size then index' - size else index'
          in
          let l' = Clause.unsafe_get clause index' in
          if
            Tribool.is_nonfalse (Assignment.Map.Cached.value l' c)
            && not (L.equal l' other_watched_literal)
          then raise_notrace (Found (index', l'))
        done ;
        if Tribool.is_false other_watched_literal_truth_value then
          Falsified clause
        else Unit (other_watched_literal, clause)
      with Found (index', new_watched_literal) ->
        watched_clause.index <- index' ;
        watched_clause.watched_literals <-
          (other_watched_literal, new_watched_literal) ;
        let watched_literals' =
          Literal.Map.remove l watched_clause watched_literals
          |> Literal.Map.add new_watched_literal watched_clause
        in
        WatchedLiteralChange (watched_clause, watched_literals')

  let watch c clause watched_literals =
    let size = Clause.size clause in
    Clause.to_iter clause
    |> Iter.map (fun l -> (l, Assignment.Map.Cached.value l c))
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
