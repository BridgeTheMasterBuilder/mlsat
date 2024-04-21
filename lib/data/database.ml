type modification = Addition of Clause.t | Deletion of Clause.t
type trace = modification Vector.vector
type t = { trace : trace; database : Clause.Watched.t Vector.vector }

let add_clause clause ({ trace; database } as db) =
  let c = Clause.Watched.to_clause clause in
  Vector.push trace (Addition c);
  Vector.push database clause;
  db

let check { database = db; _ } =
  let copy = Vector.copy db in
  Vector.uniq_sort
    (fun c1 c2 ->
      let c1 = Clause.Watched.to_clause c1 in
      let c2 = Clause.Watched.to_clause c2 in
      Array.compare Literal.compare
        (Array.sorted Literal.compare (Clause.to_array c1))
        (Array.sorted Literal.compare (Clause.to_array c2)))
    copy;
  let db_equal = Vector.length copy = Vector.length db in
  if not db_equal then
    List.iter
      (fun (c1, c2) ->
        let c1 = Clause.Watched.to_clause c1 in
        let c2 = Clause.Watched.to_clause c2 in
        Logs.debug (fun m ->
            m "[%b ]%s = %s"
              (String.equal (Clause.show c1) (Clause.show c2))
              (Clause.show c1) (Clause.show c2)))
      (List.combine_shortest
         (List.sort_uniq
            ~cmp:(fun c1 c2 ->
              let c1 = Clause.Watched.to_clause c1 in
              let c2 = Clause.Watched.to_clause c2 in
              Array.compare Literal.compare
                (Array.sorted Literal.compare (Clause.to_array c1))
                (Array.sorted Literal.compare (Clause.to_array c2)))
            (Vector.to_list db))
         (List.sort
            (fun c1 c2 ->
              let c1 = Clause.Watched.to_clause c1 in
              let c2 = Clause.Watched.to_clause c2 in
              Array.compare Literal.compare
                (Array.sorted Literal.compare (Clause.to_array c1))
                (Array.sorted Literal.compare (Clause.to_array c2)))
            (Vector.to_list db)));
  assert db_equal

let create () = { trace = Vector.create (); database = Vector.create () }
let fold f init { database; _ } = Vector.fold f init database
let get_trace { trace; _ } = trace

let simplify ({ database; trace } as db) =
  Logs.debug (fun m -> m "Simplifying formula");
  let half_length = Vector.length database / 2 in
  let open Iter in
  Vector.rev_in_place database;
  Vector.to_iter database |> take half_length
  |> iter (fun clause ->
         let c = Clause.Watched.to_clause clause in
         Vector.push trace (Deletion c));
  Vector.truncate database half_length;
  Vector.rev_in_place database;
  db
