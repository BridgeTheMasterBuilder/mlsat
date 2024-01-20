type t = {
  iter : Literal.t Iter.iter;
  size : int;
  index : int;
  watchers : int * int;
}

let of_clause c =
  {
    iter = Clause.to_iter c |> Iter.cycle;
    size = Clause.size c;
    index = 0;
    watchers = (0, 1);
  }

let update = Fun.id
