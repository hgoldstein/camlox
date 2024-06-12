module OpCode = struct
  type t =
    | Return
    | Constant
    | Negate
    | Add
    | Subtract
    | Multiply
    | Divide
    | Nil
    | True
    | False
    | Not
    | Equal
    | Greater
    | Less
    | Print
    | Pop
    | DefineGlobal
    | GetGlobal
    | SetGlobal
    | GetLocal
    | SetLocal
    | JumpIfFalse
    | Jump
    | Loop

  (* NOTE: I don't love using Obj.magic here but it's the easiest way to
   * have this converstion function w/o codegen or writing things by hand.
   *)
  let to_byte (op : t) : char =
    let c = Obj.magic op in
    let i = Char.code c in
    assert (i >= 0 && i <= 255);
    c

  let of_byte : char -> t = Obj.magic
end

type t = {
  code : char Vector.t;
  constants : Value.t Vector.t;
  lines : int Vector.t;
}

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
  Vector.write ~vec:chunk.code ~data:(OpCode.to_byte opcode);
  Vector.write ~vec:chunk.lines ~data:line

(* TODO(hgoldstein): Add some sort of int8 wrapper? *)
let write_int_unsafe ~chunk ~line ~value =
  Vector.write ~vec:chunk.code ~data:(Char.chr value);
  Vector.write ~vec:chunk.lines ~data:line

let add_constant ~chunk ~value =
  Vector.write ~vec:chunk.constants ~data:value;
  let idx = Vector.length ~vec:chunk.constants - 1 in
  assert (idx >= 0 && idx <= 255);
  idx
