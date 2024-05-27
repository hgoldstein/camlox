type t = Float of float

let print = function 
  | Float f -> Printf.printf "%g" f