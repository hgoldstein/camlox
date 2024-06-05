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
    | Not -> '\x0A'
    | Equal -> '\x0B'
    | Greater -> '\x0C'
    | Less -> '\x0D'
    | Print -> '\x0E'
    | Pop -> '\x0F'
    | DefineGlobal -> '\x10'
    | GetGlobal -> '\x11'
    | SetGlobal -> '\x12'

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
    | '\x0A' -> Ok Not
    | '\x0B' -> Ok Equal
    | '\x0C' -> Ok Greater
    | '\x0D' -> Ok Less
    | '\x0E' -> Ok Print
    | '\x0F' -> Ok Pop
    | '\x10' -> Ok DefineGlobal
    | '\x11' -> Ok GetGlobal
    | '\x12' -> Ok SetGlobal
    | c -> Error c
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
