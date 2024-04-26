type t =
  { time_limit: float
  ; verbose: bool
  ; base_num_conflicts: int
  ; grow_factor: int
  ; emit_proof: string option
  ; max_learned: int
  ; decay_interval: int
  ; decay_factor: float }
