type t = { time_limit : float; base_num_conflicts : int; grow_factor : int }

let empty = { time_limit = 0.0; base_num_conflicts = 0; grow_factor = 0 }
