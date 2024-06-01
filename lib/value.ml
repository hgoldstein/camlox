type obj = String of { chars : string }
type t = Float of float | Bool of bool | Nil | Object of obj

let print = function
  | Float f -> Printf.printf "%g" f
  | Bool b -> Printf.printf "%s" (if b then "true" else "false")
  | Nil -> Printf.printf "nil"
  | Object (String { chars }) -> Printf.printf "\"%s\"" chars

let is_falsey = function
  | Nil -> true
  | Bool b -> not b
  | Float _ -> false
  | Object _ -> false

let equal a b =
  match (a, b) with
  | Float a, Float b -> a = b
  | Bool a, Bool b -> a = b
  | Nil, Nil -> true
  | Object (String { chars = a }), Object (String { chars = b }) ->
      String.equal a b
  | _, _ -> false
