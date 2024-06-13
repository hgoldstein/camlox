open Core

type t = {
  code : char Vector.t;
  constants : value Vector.t;
  lines : int Vector.t;
}

and obj =
  | String of String_val.t
  | Function of { arity : int; chunk : t; name : String_val.t }

and value = Float of float | Bool of bool | Nil | Object of obj

let show_value = function
  | Float f -> Printf.sprintf "%g" f
  | Bool b -> Printf.sprintf "%s" (if b then "true" else "false")
  | Nil -> Printf.sprintf "nil"
  | Object (String s) -> Printf.sprintf "%s" (String_val.get s)
  | Object (Function f) -> Printf.sprintf "<fn %s>" (String_val.get f.name)

let print_value v = Printf.printf "%s" @@ show_value v

let print_line v =
  Printf.printf "%s" @@ show_value v;
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

let make () =
  {
    code = Vector.empty ();
    constants = Vector.empty ();
    lines = Vector.empty ();
  }

let write_byte ~chunk ~line ~byte =
  Vector.write ~vec:chunk.code ~data:byte;
  Vector.write ~vec:chunk.lines ~data:line

let write_op ~chunk ~line ~opcode =
  Vector.write ~vec:chunk.code ~data:(Opcode.to_byte opcode);
  Vector.write ~vec:chunk.lines ~data:line

(* TODO(hgoldstein): Add some sort of int8 wrapper? *)
let write_int_unsafe ~chunk ~line ~value =
  Vector.write ~vec:chunk.code ~data:(Char.of_int_exn value);
  Vector.write ~vec:chunk.lines ~data:line

let add_constant ~chunk ~value =
  Vector.write ~vec:chunk.constants ~data:value;
  let idx = Vector.length ~vec:chunk.constants - 1 in
  assert (idx >= 0 && idx <= 255);
  idx
