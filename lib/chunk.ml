module OpCode = struct
  type t = Return | Constant | Negate

  let to_byte : t -> char = function
    | Return -> '\x00'
    | Constant -> '\x01'
    | Negate -> '\x02'

  let of_byte : char -> (t, char) result = function
    | '\x00' -> Ok Return
    | '\x01' -> Ok Constant
    | '\x02' -> Ok Negate
    | c -> Error c
end

type t = {
  code : char Vector.t;
  constants : Value.t Vector.t;
  lines : int Vector.t;
}

let make () =
  {
    code = Vector.make ~size:0 ~elem:'\x00';
    constants = Vector.make ~size:0 ~elem:(Value.Float 0.0);
    lines = Vector.make ~size:0 ~elem:(-1);
  }

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
