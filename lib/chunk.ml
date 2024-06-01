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

  let to_byte : t -> char = function
    | Return -> '\x00'
    | Constant -> '\x01'
    | Negate -> '\x02'
    | Add -> '\x03'
    | Subtract -> '\x04'
    | Multiply -> '\x05'
    | Divide -> '\x06'
    | Nil -> '\x07'
    | True -> '\x08'
    | False -> '\x09'
    | Not -> '\x10'
    | Equal -> '\x11'
    | Greater -> '\x12'
    | Less -> '\x13'

  let of_byte : char -> (t, char) result = function
    | '\x00' -> Ok Return
    | '\x01' -> Ok Constant
    | '\x02' -> Ok Negate
    | '\x03' -> Ok Add
    | '\x04' -> Ok Subtract
    | '\x05' -> Ok Multiply
    | '\x06' -> Ok Divide
    | '\x07' -> Ok Nil
    | '\x08' -> Ok True
    | '\x09' -> Ok False
    | '\x10' -> Ok Not
    | '\x11' -> Ok Equal
    | '\x12' -> Ok Greater
    | '\x13' -> Ok Less
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
