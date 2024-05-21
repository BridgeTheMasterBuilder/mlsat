type modification = Addition of Clause.t | Deletion of Clause.t

type trace = modification CCFQueue.t

type t = {trace: trace; database: Clause.t CCFQueue.t; id: int}

let add_clause clause ({trace; database; _} as db) =
  { db with
    trace= CCFQueue.snoc trace (Addition clause)
  ; database= CCFQueue.snoc database clause }

let check {database= db; _} =
  let uniq_size =
    CCFQueue.to_iter db
    |> Iter.sort_uniq ~cmp:(fun c1 c2 ->
           Array.compare Literal.compare
             (Array.sorted Literal.compare (Clause.to_array c1))
             (Array.sorted Literal.compare (Clause.to_array c2)) )
    |> Iter.length
  in
  let db_equal = uniq_size = CCFQueue.size db in
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
            (CCFQueue.to_list db) )
         (List.sort
            (fun c1 c2 ->
              Array.compare Literal.compare
                (Array.sorted Literal.compare (Clause.to_array c1))
                (Array.sorted Literal.compare (Clause.to_array c2)) )
            (CCFQueue.to_list db) ) ) ;
  assert db_equal

let create c = {trace= CCFQueue.empty; database= CCFQueue.empty; id= c}

let fold f init {database; _} = CCFQueue.fold f init database

let get_trace {trace; _} = trace

let new_id ({id; _} as db) =
  let id' = id + 1 in
  (id, {db with id= id'})

let delete_half ({database; trace; _} as db) watched_literals clauses =
  Logs.debug (fun m -> m "Simplifying formula") ;
  let half_length = CCFQueue.size database / 2 in
  let open Iter in
  let watched_literals', clauses', trace' =
    CCFQueue.to_iter database |> rev |> take half_length
    |> fold
         (fun (watched_literals', clauses', trace') c ->
           match Watched.Clause.Map.find_opt c clauses' with
           | Some watched_clause ->
               let trace'' = CCFQueue.snoc trace (Deletion c) in
               let watched_literals'' =
                 Watched.Clause.unwatch watched_clause watched_literals'
               in
               let clause = Watched.Clause.to_clause watched_clause in
               let clauses'' = Watched.Clause.Map.remove clause clauses' in
               (watched_literals'', clauses'', trace'')
           | None ->
               (watched_literals', clauses', trace') )
         (watched_literals, clauses, trace)
  in
  let database' =
    CCFQueue.of_iter (CCFQueue.to_iter database |> take half_length)
  in
  ({db with database= database'; trace= trace'}, watched_literals', clauses')
