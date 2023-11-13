include Int

let invalid = 0
let is_negated l = l < 0
let of_int i = if i = 0 then failwith "Invalid literal" else i
let var = abs
