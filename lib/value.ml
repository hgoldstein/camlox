open Core

type str = { chars : string; hash : int }
and obj = String of String_val.t
and t = Float of float | Bool of bool | Nil | Object of obj [@@deriving show]

let print = function
  | Float f -> Printf.printf "%g" f
  | Bool b -> Printf.printf "%s" (if b then "true" else "false")
  | Nil -> Printf.printf "nil"
  | Object (String s) -> Printf.printf "\"%s\"" (String_val.get s)

let print_line v =
  print v;
  Printf.printf "\n"

let is_falsey = function
  | Nil -> true
  | Bool b -> not b
  | Float _ -> false
  | Object _ -> false

let equal a b =
  match (a, b) with
  | Float a, Float b -> Float.equal a b
  | Bool a, Bool b -> Bool.equal a b
  | Nil, Nil -> true
  | Object (String a), Object (String b) -> phys_equal a b
  | _, _ -> false
