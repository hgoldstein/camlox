let state = ref false
let on () = !state
let enable () = state := true

let simple_instruction name offset =
  Printf.printf "%s\n" name;
  offset + 1

let byte_instruction (chunk : Chunk.t) name offset =
  let byte = Char.code @@ Vector.at ~vec:chunk.code ~index:(offset + 1) in
  Printf.printf "%-16s %4d\n" name byte;
  offset + 2

let constant_instruction (chunk : Chunk.t) name offset =
  let const = Char.code @@ Vector.at ~vec:chunk.code ~index:(offset + 1) in
  Printf.printf "%-16s %4d " name const;
  Chunk.print_value @@ Vector.at ~vec:chunk.constants ~index:const;
  Printf.printf "\n";
  offset + 2

let jump_instruction (chunk : Chunk.t) name sign offset =
  let jump =
    ((Char.code @@ Vector.at ~vec:chunk.code ~index:(offset + 1)) lsl 8)
    lor (Char.code @@ Vector.at ~vec:chunk.code ~index:(offset + 2))
  in
  Printf.printf "%-16s %4d -> %d\n" name offset (offset + 3 + (sign * jump));
  offset + 3

let print_line_number (c : Chunk.t) (offset : int) =
  let f = Vector.at ~vec:c.lines in
  if offset > 0 && f ~index:offset = f ~index:(offset - 1) then
    Printf.printf "   | "
  else Printf.printf "%4d " (f ~index:offset)

let disassemble_instruction (c : Chunk.t) (offset : int) : int =
  let module Op = Opcode in
  Printf.printf "%04d " offset;
  print_line_number c offset;
  match Op.of_byte (Vector.at ~vec:c.code ~index:offset) with
  | Op.Constant -> constant_instruction c "OP_CONSTANT" offset
  | Op.Return -> simple_instruction "OP_RETURN" offset
  | Op.Negate -> simple_instruction "OP_NEGATE" offset
  | Op.Add -> simple_instruction "OP_ADD" offset
  | Op.Subtract -> simple_instruction "OP_SUBTRACT" offset
  | Op.Multiply -> simple_instruction "OP_MULTIPLY" offset
  | Op.Divide -> simple_instruction "OP_DIVIDE" offset
  | Op.Nil -> simple_instruction "OP_NIL" offset
  | Op.True -> simple_instruction "OP_TRUE" offset
  | Op.False -> simple_instruction "OP_FALSE" offset
  | Op.Not -> simple_instruction "OP_NOT" offset
  | Op.Less -> simple_instruction "OP_LESS" offset
  | Op.Greater -> simple_instruction "OP_GREATER" offset
  | Op.Equal -> simple_instruction "OP_EQUAL" offset
  | Op.Print -> simple_instruction "OP_PRINT" offset
  | Op.Pop -> simple_instruction "OP_POP" offset
  | Op.DefineGlobal -> constant_instruction c "OP_DEFINE_GLOBAL" offset
  | Op.GetGlobal -> constant_instruction c "OP_GET_GLOBAL" offset
  | Op.SetGlobal -> constant_instruction c "OP_SET_GLOBAL" offset
  | Op.GetLocal -> byte_instruction c "OP_GET_LOCAL" offset
  | Op.SetLocal -> byte_instruction c "OP_SET_LOCAL" offset
  | Op.JumpIfFalse -> jump_instruction c "OP_JUMP_IF_FALSE" 1 offset
  | Op.Jump -> jump_instruction c "OP_JUMP" 1 offset
  | Op.Loop -> jump_instruction c "OP_LOOP" (-1) offset
  | Op.Call -> failwith "unimplemented"

let disassemble_chunk (c : Chunk.t) (name : string) =
  Printf.printf "== %s ==\n" name;
  let rec aux offset =
    if offset < Vector.length ~vec:c.code then
      aux @@ disassemble_instruction c offset
    else ()
  in
  aux 0
