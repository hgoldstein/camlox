open Core

type t = {
  code : char Vector.t;
  constants : value_ Vector.t;
      (* This is a `value_` not a `value` as everything inside ought to be constant, though they may contain references *)
  lines : int Vector.t;
}

and function_ = {
  arity : int;
  chunk : t;
  name : String_val.t option;
  mutable upvalue_count : int;
}

and closure = { function_ : function_ }

and obj =
  | String of String_val.t
  | Function of function_
  | Closure of closure
  | Native of (int -> value list -> value)

and value_ = Float of float | Bool of bool | Nil | Object of obj
and value = value_ ref

let float v = ref (Float v)
let bool b = ref (Bool b)
let obj o = ref (Object o)

let show_value =
  let show_function ({ name; _ } : function_) =
    match name with
    | Some f -> Printf.sprintf "<fn %s>" (String_val.get f)
    | None -> "<script>"
  in
  function
  | Float f -> Printf.sprintf "%g" f
  | Bool b -> Printf.sprintf "%s" (if b then "true" else "false")
  | Nil -> Printf.sprintf "nil"
  | Object (String s) -> Printf.sprintf "%s" (String_val.get s)
  | Object (Function f) -> show_function f
  | Object (Closure { function_ }) -> show_function function_
  | Object (Native _) -> "<native>"

let print_value v = Printf.printf "%s" @@ show_value v

let print_line v =
  Printf.printf "%s" @@ show_value v;
  Printf.printf "\n"

let is_falsey v =
  match !v with
  | Nil -> true
  | Bool b -> not b
  | Float _ -> false
  | Object _ -> false

let equal a b =
  match (!a, !b) with
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
