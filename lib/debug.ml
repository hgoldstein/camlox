let state = ref false
let on () = !state
let enable () = state := true

let simple_instruction name offset =
  Printf.printf "%s\n" name;
  offset + 1

let constant_instruction (chunk : Chunk.t) name offset =
  let const = Char.code @@ Vector.at ~vec:chunk.code ~index:(offset + 1) in
  Printf.printf "%-16s %4d " name const;
  Value.print @@ Vector.at ~vec:chunk.constants ~index:const;
  Printf.printf "\n";
  offset + 2

let print_line_number (c : Chunk.t) (offset : int) =
  let f = Vector.at ~vec:c.lines in
  if offset > 0 && f ~index:offset = f ~index:(offset - 1) then
    Printf.printf "   | "
  else Printf.printf "%4d " (f ~index:offset)

let disassemble_instruction (c : Chunk.t) (offset : int) : int =
  let module Op = Chunk.OpCode in
  Printf.printf "%04d " offset;
  print_line_number c offset;
  match Op.of_byte (Vector.at ~vec:c.code ~index:offset) with
  | Ok Op.Constant -> constant_instruction c "OP_CONSTANT" offset
  | Ok Op.Return -> simple_instruction "OP_RETURN" offset
  | Ok Op.Negate -> simple_instruction "OP_NEGATE" offset
  | Ok Op.Add -> simple_instruction "OP_ADD" offset
  | Ok Op.Subtract -> simple_instruction "OP_SUBTRACT" offset
  | Ok Op.Multiply -> simple_instruction "OP_MULTIPLY" offset
  | Ok Op.Divide -> simple_instruction "OP_DIVIDE" offset
  | Error c ->
      Printf.printf "Unknown opcode %d" (Char.code c);
      offset + 1

let disassemble_chunk (c : Chunk.t) (name : string) =
  Printf.printf "== %s ==\n" name;
  let rec aux offset =
    if offset < Vector.length ~vec:c.code then
      aux @@ disassemble_instruction c offset
    else ()
  in
  aux 0
