type t = Float of float

let print = function Float f -> Printf.printf "%g" f
let add l r = match (l, r) with Float x, Float y -> Float (x +. y)
let sub l r = match (l, r) with Float x, Float y -> Float (x -. y)
let div l r = match (l, r) with Float x, Float y -> Float (x /. y)
let mul l r = match (l, r) with Float x, Float y -> Float (x *. y)
