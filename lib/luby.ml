type t = {unit_run: int; phase: int; grow: int; max: int}

let create unit_run grow = {unit_run; phase= 0; grow; max= grow}

let next ({unit_run; phase; grow; max} as l) =
  match phase with
  | 0 | 1 ->
      (unit_run, {l with phase= phase + 1})
  | step ->
      if step = max then (unit_run * step, {l with phase= 0; max= max * grow})
      else (unit_run * step, {l with phase= step * grow})
