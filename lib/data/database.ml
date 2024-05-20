type modification = Addition of Clause.t | Deletion of Clause.t

type trace = modification Vector.vector

type t = {trace: trace; database: Clause.t Vector.vector; id: int}

let add_clause clause ({trace; database; _} as db) =
  Vector.push trace (Addition clause) ;
  Vector.push database clause ;
  db

let check {database= db; _} =
  let copy = Vector.copy db in
  Vector.uniq_sort
    (fun c1 c2 ->
      Array.compare Literal.compare
        (Array.sorted Literal.compare (Clause.to_array c1))
        (Array.sorted Literal.compare (Clause.to_array c2)) )
    copy ;
  let db_equal = Vector.length copy = Vector.length db in
  if not db_equal then
    List.iter
      (fun (c1, c2) ->
        Logs.debug (fun m ->
            m "[%b ]%s = %s"
              (String.equal (Clause.show c1) (Clause.show c2))
              (Clause.show c1) (Clause.show c2) ) )
      (List.combine_shortest
         (List.sort_uniq
            ~cmp:(fun c1 c2 ->
              Array.compare Literal.compare
                (Array.sorted Literal.compare (Clause.to_array c1))
                (Array.sorted Literal.compare (Clause.to_array c2)) )
            (Vector.to_list db) )
         (List.sort
            (fun c1 c2 ->
              Array.compare Literal.compare
                (Array.sorted Literal.compare (Clause.to_array c1))
                (Array.sorted Literal.compare (Clause.to_array c2)) )
            (Vector.to_list db) ) ) ;
  assert db_equal

let create c = {trace= Vector.create (); database= Vector.create (); id= c}

let fold f init {database; _} = Vector.fold f init database

let get_trace {trace; _} = trace

let new_id ({id; _} as db) =
  let id' = id + 1 in
  (id, {db with id= id'})

let delete_half ({database; trace; _} as db) watched_literals clauses =
  Logs.debug (fun m -> m "Simplifying formula") ;
  let half_length = Vector.length database / 2 in
  let open Iter in
  Vector.rev_in_place database ;
  let watched_literals', clauses' =
    Vector.to_iter database |> take half_length
    |> fold
         (fun (watched_literals', clauses') c ->
           match Watched.Clause.Map.find_opt c clauses' with
           | Some watched_clause ->
               Vector.push trace (Deletion c) ;
               let watched_literals'' =
                 Watched.Clause.unwatch watched_clause watched_literals'
               in
               let clause = Watched.Clause.to_clause watched_clause in
               let clauses'' = Watched.Clause.Map.remove clause clauses' in
               (watched_literals'', clauses'')
           | None ->
               (watched_literals', clauses') )
         (watched_literals, clauses)
  in
  Vector.truncate database half_length ;
  Vector.rev_in_place database ;
  (db, watched_literals', clauses')
