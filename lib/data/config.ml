type t = {
  time_limit : float;
  verbose : bool;
  base_num_conflicts : int;
  grow_factor : int;
  emit_proof : string option;
}

let empty () =
  {
    time_limit = 0.0;
    verbose = false;
    base_num_conflicts = 0;
    grow_factor = 0;
    emit_proof = None;
  }
