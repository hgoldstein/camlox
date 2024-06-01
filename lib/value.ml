type t = Float of float | Bool of bool | Nil

let print = function
  | Float f -> Printf.printf "%g" f
  | Bool b -> Printf.printf "%s" (if b then "true" else "false")
  | Nil -> Printf.printf "nil"

let is_falsey = function Nil -> true | Bool b -> not b | Float _ -> false

let equal a b =
  match (a, b) with
  | Float a, Float b -> a = b
  | Bool a, Bool b -> a = b
  | Nil, Nil -> true
  | _, _ -> false
