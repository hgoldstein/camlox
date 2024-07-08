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

and closure = { function_ : function_; upvalues : value Array.t }
and class_ = { name : String_val.t }
and instance_ = { class_ : class_; fields : value_ Table.t }

and value_ =
  | Float of float
  | Bool of bool
  | Nil
  | String of String_val.t
  | Function of function_
  | Closure of closure
  | Native of (int -> value list -> value)
  | Class of class_
  | Instance of instance_

and value = value_ ref

let float v = ref (Float v)
let bool b = ref (Bool b)
let closure o = ref (Closure o)
let cls o = ref (Class o)
let instance o = ref (Instance o)

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
  | String s -> Printf.sprintf "%s" (String_val.get s)
  | Function f -> show_function f
  | Closure { function_; _ } -> show_function function_
  | Native _ -> "<native>"
  | Class c -> Printf.sprintf "%s" (String_val.get c.name)
  | Instance i -> Printf.sprintf "%s instance" (String_val.get i.class_.name)

let print_value v = Printf.printf "%s" @@ show_value v

let print_line v =
  Printf.printf "%s" @@ show_value v;
  Printf.printf "\n"

let is_falsey v =
  match !v with
  | Nil -> true
  | Bool b -> not b
  | Float _ | String _ | Function _ | Closure _ | Native _ | Class _
  | Instance _ ->
      false

let equal a b =
  match (!a, !b) with
  | Float a, Float b -> Float.equal a b
  | Bool a, Bool b -> Bool.equal a b
  | Nil, Nil -> true
  | String a, String b -> phys_equal a b
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
